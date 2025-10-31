library(readr)
library(dplyr)
library(stringr)

`%||%` <- function(a, b) ifelse(is.null(a) | is.na(a), b, a)

SRC_DISPLAY <- c(
  bars               = "Bar",
  restaurant         = "Restaurant",
  milktea_juice      = "Milktea / Juice",
  cafe_brunch_bakery = "Cafe / Bakery"
)

KNOWN_SRCS <- c(
  bars               = "bars.csv",
  restaurant         = "restaurant.csv",
  milktea_juice      = "milk_juice.csv",
  cafe_brunch_bakery = "cafe.csv"
)

col_map <- list(
  name         = c("name"),
  image        = c("image","pictures","img","photo"),
  phone        = c("phone","tel","telephone"),
  address      = c("address","formatted_address"),
  website      = c("website","url","link"),
  rating       = c("rating","score"),
  price_level  = c("price_level","price"),
  category     = c("category","type","types"),
  description  = c("description","desc","editorialsummary"),
  place_id     = c("place_id","placeid","pid"),
  lat          = c("lat","latitude"),
  lon          = c("lon","lng","longitude"),
  openinghours = c("openinghour","openinghours","hours")
)

find_csv_for <- function(src_key) {
  cand <- KNOWN_SRCS[[src_key]]
  if (!is.null(cand) && file.exists(cand)) return(cand)
  hits <- list.files(pattern = paste0(src_key, ".*\\.csv$"), ignore.case = TRUE)
  if (length(hits)) return(hits[[1]])
  anycsv <- list.files(pattern = "\\.csv$")
  if (length(anycsv)) return(anycsv[[1]]) else return(NA_character_)
}

read_one_source <- function(src_key) {
  csv <- find_csv_for(src_key)
  if (is.na(csv)) return(NULL)
  df <- suppressMessages(readr::read_csv(csv, show_col_types = FALSE))
  if (!nrow(df)) return(NULL)
  names(df) <- tolower(names(df))
  get_col <- function(key){
    hits <- intersect(col_map[[key]], names(df))
    if (length(hits)) hits[1] else NA_character_
  }
  out <- df %>%
    mutate(
      name         = .data[[get_col("name")]],
      image        = if (!is.na(get_col("image"))) .data[[get_col("image")]] else NA,
      phone        = if (!is.na(get_col("phone"))) .data[[get_col("phone")]] else NA,
      address      = if (!is.na(get_col("address"))) .data[[get_col("address")]] else NA,
      website      = if (!is.na(get_col("website"))) .data[[get_col("website")]] else NA,
      rating       = suppressWarnings(as.numeric(.data[[get_col("rating")]])),
      price_level  = if (!is.na(get_col("price_level"))) .data[[get_col("price_level")]] else NA,
      category     = if (!is.na(get_col("category"))) .data[[get_col("category")]] else NA,
      description  = if (!is.na(get_col("description"))) .data[[get_col("description")]] else NA,
      place_id     = if (!is.na(get_col("place_id"))) .data[[get_col("place_id")]] else NA,
      lat          = suppressWarnings(as.numeric(.data[[get_col("lat")]])),
      lon          = suppressWarnings(as.numeric(.data[[get_col("lon")]])),
      openinghours = if (!is.na(get_col("openinghours"))) .data[[get_col("openinghours")]] else NA,
      src          = src_key
    ) %>%
    mutate(
      postcode    = stringr::str_extract(address %||% "", "\\b\\d{4}\\b"),
      price_level = stringr::str_to_upper(as.character(price_level)),
      category    = as.character(category)
    )
  out
}

all_src_keys <- names(SRC_DISPLAY)
datas <- lapply(all_src_keys, read_one_source)
all   <- bind_rows(datas)

sample <- all %>% filter(name == "11 Inch Pizza") %>% slice(1)
print(str(sample))
