library(tidyverse)
library(httr)
library(rvest)
library(janitor)
library(jsonlite)

DATA_DIR <- "data"
if (!dir.exists(DATA_DIR)) dir.create(DATA_DIR)

scrap_medalist <- function(athlete_id) {
  url <- paste0("https://www.olympedia.org/athletes/", athlete_id)
  tryCatch({
    Sys.sleep(runif(1, 5, 15))
    res <- GET(url, timeout(15), add_headers(`User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) Chrome/120.0.0.0"))
    if (status_code(res) != 200) return(NULL)
    
    html <- read_html(res)
    bio_table <- html %>% html_element('table.biodata') %>% html_table(header = FALSE)
    
    extract_bio <- function(label) {
      val <- bio_table %>% filter(X1 == label) %>% pull(X2)
      if(length(val) == 0) return(NA) else return(val)
    }
    
    medal_data <- html %>% html_element('table.table') %>% html_table() %>% clean_names() %>%
      mutate(g = if_else(games != "", games, NA_character_),
             n = if_else(games != "", noc_team, NA_character_),
             d = if_else(games != "", discipline_sport_event, NA_character_)) %>%
      fill(g, n, d) %>% filter(games == "" & medal != "") %>%
      transmute(game = g, noc = n, discipline = d, event = discipline_sport_event, medal = medal)
    
    return(list(id = as.character(athlete_id), 
                name = extract_bio('Used name'), 
                born = extract_bio('Born'), 
                measurements = extract_bio('Measurements'), 
                medals = medal_data))
  }, error = function(e) return(NULL))
}

id_list <- read_csv("olympedia_medalist_id.csv")$id
existing_ids <- list.files(DATA_DIR, pattern = "*.json", full.names = TRUE) %>%
  map(~fromJSON(.x, simplifyVector = FALSE)) %>% flatten() %>% map_chr("id", .default = "")

target_ids <- setdiff(as.character(id_list), existing_ids)

if (length(target_ids) > 0) {
  walk(head(target_ids, 15), function(aid) {
    bucket_num <- as.numeric(aid) %% 100
    file_path <- file.path(DATA_DIR, sprintf("bucket_%02d.json", bucket_num))
    current_data <- if (file.exists(file_path)) fromJSON(file_path, simplifyVector = FALSE) else list()
    
    res <- scrap_medalist(aid)
    if (!is.null(res)) {
      current_data[[length(current_data) + 1]] <- res
      write_json(current_data, file_path, auto_unbox = TRUE, pretty = TRUE)
      message(paste("Success:", aid))
    }
  })
}
