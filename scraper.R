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
    
    res <- GET(url, timeout(15), 
               add_headers(`User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) Chrome/120.0.0.0"))
    
    if (status_code(res) != 200) return(NULL)
    
    html <- read_html(res)
    
    bio_table <- html %>% html_element('table.biodata') %>% html_table(header = FALSE)
    
    extract_field <- function(field_name) {
      val <- bio_table %>% filter(X1 == field_name) %>% pull(X2)
      if(length(val) == 0) return(NA_character_) else return(val)
    }
    
    name <- extract_field('Used name')
    born <- extract_field('Born')
    measurements <- extract_field('Measurements')
    
    raw_table <- html %>% html_element('table.table') %>% html_table() %>% clean_names()
    
    if(!"games" %in% names(raw_table)) return(NULL)
    
    medal_data <- raw_table %>%
      mutate(
        game_tmp = if_else(games != "", games, NA_character_),
        noc_tmp = if_else(games != "", noc_team, NA_character_),
        discipline_tmp = if_else(games != "", discipline_sport_event, NA_character_)
      ) %>%
      fill(game_tmp, noc_tmp, discipline_tmp, .direction = "down") %>%
      filter(games == "" & medal != "") %>% 
      transmute(
        game = game_tmp,
        noc = noc_tmp,
        discipline = discipline_tmp,
        event = discipline_sport_event,
        medal = medal
      )
    
    return(list(
      id = as.character(athlete_id),
      name = name,
      born = born,
      measurements = measurements,
      medals = medal_data
    ))
    
  }, error = function(e) {
    message(paste("Error with ID", athlete_id, ":", e$message))
    return(NULL)
  })
}

update_bucket <- function(athlete_id) {
  bucket_num <- athlete_id %% 100
  file_path <- file.path(DATA_DIR, sprintf("bucket_%02d.json", bucket_num))
  
  current_data <- if (file.exists(file_path)) fromJSON(file_path, simplifyVector = FALSE) else list()
  
  if (any(map_chr(current_data, "id", .default = "") == as.character(athlete_id))) return(FALSE)
  
  result <- scrap_medalist(athlete_id)
  if (!is.null(result)) {
    current_data[[length(current_data) + 1]] <- result
    write_json(current_data, file_path, auto_unbox = TRUE, pretty = TRUE)
    return(TRUE)
  }
  return(FALSE)
}

id_list <- read_csv("olympedia_medalist_id.csv", show_col_types = FALSE)$id
json_files <- list.files(DATA_DIR, pattern = "bucket_.*\\.json", full.names = TRUE)

if (length(json_files) > 0) {
  existing_ids <- json_files %>%
    map(~fromJSON(.x, simplifyVector = FALSE)) %>%
    flatten() %>%
    map_chr("id", .default = "")
} else {
  existing_ids <- character(0)
}

target_ids <- setdiff(as.character(id_list), existing_ids)

if (length(target_ids) > 0) {
  message(paste("남은 대상:", length(target_ids), "명. 이번 회차 수집을 시작합니다."))
  walk(head(target_ids, 15), ~{
    if(update_bucket(as.numeric(.x))) message(paste("성공:", .x))
  })
} else {
  message("축하합니다! 모든 수집이 완료되었습니다.")
}
