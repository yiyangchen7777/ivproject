
# =========================================================
# app.R 最终版：Welcome + 主页面 + suspendWhenHidden 修复 + Navbar(Map/Route/Detail)
# =========================================================

library(shiny)
library(shinyjs)
library(ggplot2)
library(readr)
library(dplyr)
library(stringr)
library(bslib)
library(leaflet)
library(lubridate)
library(tools)
library(htmltools)
library(jsonlite)
library(geosphere)
library(htmlwidgets)
library(shinydashboard)
library(tidyr)
library(DT)

# 如果有 tableau 脚本就加载（没有也不会报错）
if (file.exists("tableau-in-shiny-v1.2.R")) {
  source("tableau-in-shiny-v1.2.R")
}
if (file.exists("route_module.R")) {
  source("route_module.R")
}

# ----------------- 基础配置 & 数据 -----------------
POSTCODE_FILTER <- NA_character_
`%||%` <- function(a, b) ifelse(is.null(a) | is.na(a), b, a)
scalar_value <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA)
  if (is.list(x)) {
    if (length(x) == 0) return(NA)
    x <- x[[1]]
  }
  if (length(x) == 0) return(NA)
  if (length(x) > 1) x <- x[1]
  x
}
scalar_chr <- function(x, default = "") {
  val <- scalar_value(x)
  if (is.null(val) || length(val) == 0 || (is.atomic(val) && length(val) == 1 && is.na(val))) {
    return(default)
  }
  val_chr <- as.character(val)[1]
  if (!nzchar(val_chr) || is.na(val_chr)) default else val_chr
}

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

for (dirnm in names(SRC_DISPLAY)) {
  if (dir.exists(dirnm)) shiny::addResourcePath(dirnm, normalizePath(dirnm))
}

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
if (!is.na(POSTCODE_FILTER)) {
  all <- dplyr::filter(all, is.na(postcode) | postcode == POSTCODE_FILTER)
}
stopifnot(nrow(all) > 0)

# ----------------- Map 数据 -----------------
read_places <- function(path, type_label) {
  df <- read.csv(path, stringsAsFactors = FALSE)
  df$type <- type_label
  df
}

df_cafe       <- read_places("cafe_brunch_bakery_desc.csv", "Cafe")
df_bars       <- read_places("melbourne_cbd_bars.csv", "Bar")
df_milktea    <- read_places("milktea_juice_english_clean.csv", "Milktea")
df_restaurant <- read_places("restaurant_english_clean_desc.csv", "Restaurant")

places <- bind_rows(df_cafe, df_bars, df_milktea, df_restaurant) %>%
  filter(!is.na(lat), !is.na(lon)) %>%
  filter(!name %in% c(
    "Sobo Japanese VCCC",
    "226 Sushi& Kimbap",
    "EDWIN WINE BAR AND CELLAR",
    "Edwin's Cafe",
    "Tastes Of Senegal",
    "90 Secondi",
    "Salsa's",
    "Woolworths Metro City North"
  ))

is_open_today_now <- function(hours_str) {
  if (is.null(hours_str) || is.na(hours_str) || trimws(hours_str) == "") return(FALSE)
  if (grepl("24", hours_str, ignore.case = TRUE) && grepl("hour", hours_str, ignore.case = TRUE)) return(TRUE)
  today <- weekdays(Sys.Date())
  m <- stringr::str_extract(hours_str, paste0(today, ":[^|]+"))
  if (is.na(m)) return(FALSE)
  if (grepl("Closed", m, ignore.case = TRUE)) return(FALSE)
  m <- gsub("\u2013|\u2014|–|—|to", "-", m)
  time_pair <- stringr::str_extract(m, "\\d{1,2}:\\d{2}\\s*(AM|PM)\\s*[-]\\s*\\d{1,2}:\\d{2}\\s*(AM|PM)")
  if (is.na(time_pair)) return(FALSE)
  parts <- unlist(strsplit(time_pair, "-"))
  if (length(parts) < 2) return(FALSE)

  parse_safe <- function(x) tryCatch(
    lubridate::parse_date_time(x, orders = "I:M p"),
    error = function(e) NA
  )

  open_t  <- parse_safe(trimws(parts[1]))
  close_t <- parse_safe(trimws(parts[2]))
  now_t   <- parse_safe(format(Sys.time(), "%I:%M %p"))

  if (any(is.na(c(open_t, close_t, now_t)))) return(FALSE)
  if (close_t < open_t) now_t >= open_t | now_t <= close_t else now_t >= open_t & now_t <= close_t
}

# ===================== Opening Hours + Helper =====================
price_map <- c("FREE"=0, "INEXPENSIVE"=1, "MODERATE"=2, "EXPENSIVE"=3, "VERY_EXPENSIVE"=4)
price_range_map <- c(
  "FREE"           = "$0",
  "INEXPENSIVE"    = "$10–$20",
  "MODERATE"       = "$20–$40",
  "EXPENSIVE"      = "$40–$70",
  "VERY_EXPENSIVE" = "$70–$120+"
)
price_range <- function(x){
  lvl <- as.character(x) |> stringr::str_to_upper() |> stringr::str_replace_all("^PRICE_LEVEL_", "")
  out <- price_range_map[lvl]; out[is.na(out) | !nzchar(out)] <- "No Price Info"; out
}
all <- all %>% mutate(price_num = unname(price_map[price_level]))

.day_keys <- c("mon","tue","wed","thu","fri","sat","sun")

# ===================== 你的原始营业时间函数（原样保留） =====================
parse_opening_hours <- function(txt) {
  if (is.na(txt) || !nzchar(txt))
    return(data.frame(day=character(), start=character(), end=character()))
  x <- gsub("[\u00A0\u2009\u202F\u2002-\u200A]", " ", txt, perl=TRUE)
  x <- gsub("[\u2012-\u2015\u2212\uFE58\uFE63\uFF0D\u2013\u2014]", "-", x, perl=TRUE)
  x <- trimws(x)
  segs <- unlist(strsplit(x, "\\|"))
  out <- list()
  has_day <- grepl("Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday", x, ignore.case=TRUE)
  if (!has_day) {
    for (day in .day_keys) {
      matches <- regmatches(
        x,
        gregexpr("\\d{1,2}:\\d{2}\\s*(?:AM|PM)?\\s*[-–]\\s*\\d{1,2}:\\d{2}\\s*(?:AM|PM)?",
                 x, ignore.case=TRUE)
      )[[1]]
      if (length(matches) == 0) next
      for (t in matches) {
        parts <- unlist(strsplit(gsub("\\s", "", t), "[-–]"))
        to24 <- function(tt) {
          tt <- trimws(tt)
          if (grepl("AM|PM", tt, ignore.case=TRUE)) {
            parsed <- lubridate::parse_date_time(tt, orders=c("I:%M%p"), tz="UTC")
            format(parsed, "%H:%M")
          } else {
            nums <- strsplit(tt, ":", fixed=TRUE)[[1]]
            h <- as.numeric(nums[1]); m <- as.numeric(nums[2])
            if (h < 5) h <- h + 12
            sprintf("%02d:%02d", h, m)
          }
        }
        s <- to24(parts[1]); e <- to24(parts[2])
        out[[length(out)+1]] <- data.frame(day=day, start=s, end=e)
      }
    }
  } else {
    for (seg in segs) {
      seg <- trimws(seg)
      if (!nzchar(seg)) next
      m <- regexpr("^(Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday)", seg, ignore.case=TRUE)
      if (m[1] == -1) next
      day <- tolower(substr(regmatches(seg, m), 1, 3))
      if (grepl("closed", seg, ignore.case=TRUE)) next
      matches <- regmatches(
        seg,
        gregexpr("\\d{1,2}:\\d{2}\\s*(?:AM|PM)?\\s*[-–]\\s*\\d{1,2}:\\d{2}\\s*(?:AM|PM)?",
                 seg, ignore.case=TRUE)
      )[[1]]
      if (length(matches) == 0) next
      for (t in matches) {
        parts <- unlist(strsplit(gsub("\\s", "", t), "[-–]"))
        to24 <- function(tt) {
          tt <- trimws(tt)
          if (grepl("AM|PM", tt, ignore.case=TRUE)) {
            parsed <- lubridate::parse_date_time(tt, orders=c("I:%M%p"), tz="UTC")
            format(parsed, "%H:%M")
          } else {
            nums <- strsplit(tt, ":", fixed=TRUE)[[1]]
            h <- as.numeric(nums[1]); m <- as.numeric(nums[2])
            if (h < 5) h <- h + 12
            sprintf("%02d:%02d", h, m)
          }
        }
        s <- to24(parts[1]); e <- to24(parts[2])
        out[[length(out)+1]] <- data.frame(day=day, start=s, end=e)
      }
    }
  }
  if (!length(out))
    return(data.frame(day=character(), start=character(), end=character()))
  do.call(rbind, out)
}

is_open_at <- function(df, when, tz="Australia/Melbourne") {
  if (nrow(df) == 0) return(FALSE)
  when <- with_tz(as.POSIXct(when, tz=tz), tz)
  dow <- .day_keys[wday(when, week_start = 1)]
  ref <- as_date(when)
  check_day <- function(day_key, ref_date) {
    dd <- df[df$day == day_key, , drop=FALSE]
    if (!nrow(dd)) return(FALSE)
    any(vapply(seq_len(nrow(dd)), function(i) {
      s <- dd$start[i]; e <- dd$end[i]
      overnight <- (e < s && e != "24:00")
      start_dt <- ymd_hm(paste(ref_date, s), tz=tz)
      end_dt <- if (overnight)
        ymd_hm(paste(ref_date + days(1), e), tz=tz)
      else
        ymd_hm(paste(ref_date, e), tz=tz)
      when >= start_dt & when <= end_dt
    }, logical(1)))
  }
  today_open <- check_day(dow, ref)
  prev_day <- .day_keys[ifelse(match(dow, .day_keys) - 1 >= 1,
                               match(dow, .day_keys) - 1, 7)]
  yesterday_open <- check_day(prev_day, ref - days(1))
  today_open || yesterday_open
}

today_hours_and_next_open <- function(df, now=with_tz(Sys.time(),"Australia/Melbourne")){
  if (nrow(df)==0) return(list(today="No data", next_open=""))
  dow  <- .day_keys[wday(now, week_start=1)]
  tz   <- "Australia/Melbourne"; ref  <- as_date(now, tz=tz)
  dd <- df[df$day==dow, , drop=FALSE]
  today <- if (nrow(dd)) paste(apply(dd, 1, \(r) paste0(r["start"], "–", r["end"])), collapse=", ") else "Closed"
  next_open <- ""
  for (k in 0:7){
    day_key <- .day_keys[((match(dow,.day_keys)-1 + k) %% 7) + 1]
    dd2 <- df[df$day==day_key, , drop=FALSE]
    if (!nrow(dd2)) next
    to_num <- function(t){
      if (t == "24:00") return(24)
      nums <- strsplit(t, ":", fixed = TRUE)[[1]]
      as.numeric(nums[1]) + as.numeric(nums[2])/60
    }
    start_min <- min(sapply(dd2$start, to_num))
    hh <- floor(start_min); mm <- round((start_min - hh)*60)
    next_open <- sprintf("%s %02d:%02d", tools::toTitleCase(day_key), hh, mm)
    break
  }
  list(today=today, next_open=next_open)
}

time_to_num <- function(t){
  t <- as.character(t)
  ifelse(t=="24:00", 24,
         {
           sp <- strsplit(t, ":", fixed = TRUE)
           vapply(sp, function(x) as.numeric(x[1]) + as.numeric(x[2])/60, numeric(1))
         })
}

expand_overnight_for_plot <- function(df){
  if (!nrow(df)) return(df)
  res <- list()
  for (i in seq_len(nrow(df))){
    d  <- df$day[i]
    s  <- df$start[i]
    e  <- df$end[i]
    sn <- time_to_num(s); en <- time_to_num(e)
    if (e != "24:00" && en < sn){
      res[[length(res)+1]] <- data.frame(day=d, start_num=sn, end_num=24)
      res[[length(res)+1]] <- data.frame(day=.day_keys[((match(d,.day_keys)) %% 7) + 1], start_num=0, end_num=en)
    } else {
      res[[length(res)+1]] <- data.frame(day=d, start_num=sn, end_num=en)
    }
  }
  do.call(rbind, res)
}

# ----------------- UI：welcome + navbar 主页面 -----------------
ui <- fluidPage(
  useShinyjs(),
  theme = bs_theme(version = 5, base_font = font_google("Poppins"), heading_font = font_google("Poppins")),
  tags$head(
    if (exists("setUpTableauInShiny")) setUpTableauInShiny(),
    
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;500;600;700&display=swap');
      body {
        font-family: 'Poppins', sans-serif;
        margin:0; padding:0;
        overflow-x:hidden;
        background: linear-gradient(135deg,#cfe2ff,#dbeafe,#f8fafc);
      }
      h1, h2, h3, h4, h5, h6 {
        font-family: 'Poppins', sans-serif;
        font-weight:600;
        color:#1f2933;
      }
      .navbar-brand,
      .navbar-nav > li > a,
      .navbar-nav .nav-link,
      .nav-tabs > li > a,
      .nav-tabs .nav-link {
        font-family: 'Poppins', sans-serif !important;
        font-weight:500;
        letter-spacing:0.3px;
        font-size:16px !important;
      }
      #welcome {
        position: relative; z-index: 3; height:100vh;
        display:flex; flex-direction:column; justify-content:center; align-items:center;
        opacity: 1; pointer-events: auto; transition: opacity 0.8s ease;
      }
      #mainApp {
        position: relative; z-index: 1; padding:20px;
        opacity: 0; pointer-events: none; transition: opacity 0.8s ease;
      }
      .glass {
        background: rgba(255,255,255,0.12);
        box-shadow: 0 12px 40px rgba(31,38,135,0.25);
        backdrop-filter: blur(20px) saturate(140%);
        -webkit-backdrop-filter: blur(20px) saturate(140%);
        border: 1px solid rgba(255,255,255,0.25);
        border-radius:24px; padding:32px 48px; text-align:center;
        width:90%; max-width:1200px; height:80vh; display:flex; flex-direction:column; align-items:center; justify-content:center;
      }
      .welcome-title {
        font-family:'Poppins',sans-serif;
        font-size:52px; font-weight:700; letter-spacing:0.5px;
        background: linear-gradient(90deg,#4f46e5,#2563eb,#0ea5e9);
        -webkit-background-clip:text; -webkit-text-fill-color:transparent;
        margin-bottom:24px;
      }
      .enter-btn { margin-top:24px; padding:12px 26px; font-size:18px; background:#3478f6; color:white; border:none; border-radius:10px; cursor:pointer; box-shadow:0 4px 10px rgba(52,120,246,0.25); transition:all .3s; }
      .enter-btn:hover { background:#265ed2; box-shadow:0 6px 14px rgba(52,120,246,0.35); }
      .hero-card {background:#fff; border-radius:16px; box-shadow:0 6px 20px rgba(0,0,0,0.07); padding:16px;}
      .meta-card {background:#fff; border-radius:12px; box-shadow:0 4px 16px rgba(0,0,0,0.06); padding:12px;}
      .desc-card {background:#fff; border-radius:12px; box-shadow:0 4px 16px rgba(0,0,0,0.06); padding:16px;}
      .kpi-card  {background:#fff; border-radius:12px; box-shadow:0 4px 16px rgba(0,0,0,0.06); padding:14px;}
      .shop-img  {width:100%; height:320px; object-fit:cover; border-radius:12px; background:#f2f2f2;}
      .kpi {font-weight:600; margin-right:16px;}
      .muted {color:#667085;}
      .rating-wrap {display:flex; flex-direction:column; gap:6px;}
      .rating-label {font-weight:600;}
      .progress-outer {width:100%; height:16px; border-radius:999px; background:#f0f2f5; overflow:hidden;}
      .progress-inner {height:100%; width:0%; border-radius:999px; background:linear-gradient(90deg, #6ba8ff, #3478f6); transition:width 900ms ease;}
    ")),
    tags$style(HTML("
        .hero-card,
        .meta-card,
        .desc-card,
        .kpi-card {
          background: #f3f3f3 !important;
        }

        .hero-card, .meta-card, .desc-card, .kpi-card {
          box-shadow: 0 4px 14px rgba(0, 0, 0, 0.05) !important;
        }
      ")),
    
    
    tags$script(HTML("
      function goToMain(){
        $('#welcome').css({opacity:0,pointerEvents:'none'});
        setTimeout(function(){$('#welcome').remove();},900);
        $('#mainApp').css({opacity:1,pointerEvents:'auto'});
        setTimeout(function(){
          if(window.HTMLWidgets&&HTMLWidgets.find){
            var maps=HTMLWidgets.find('.leaflet.html-widget');
            maps.forEach(function(m){if(m&&m.instance&&m.instance.invalidateSize){m.instance.invalidateSize();}});
          }
        },500);
        setTimeout(function(){
          $('.carousel').each(function(){ $(this).trigger('slid.bs.carousel'); });
        },600);
      }
      Shiny.addCustomMessageHandler('peerCaptionText', function(x){
        var el = document.getElementById('peerCaption');
        if (el) el.textContent = x.text || '';
      });
      $(document).ready(function(){
        $('iframe').css('opacity', 0);
        setTimeout(function(){ $('iframe').animate({opacity: 1}, 1500); }, 400);
      });
    "))
    ,
    HTML('
    <style>
      .main-block { margin-bottom: 12px; }
      .sub-option {
        margin-left: 10px; margin-top: 4px; display: none;
        max-height: 150px; overflow-y: auto; border-left: 2px solid #ddd; padding-left: 8px;
      }
      .sub-option::-webkit-scrollbar { width: 5px; }
      .sub-option::-webkit-scrollbar-thumb { background-color: #ccc; border-radius: 3px; }
      .sub-option::-webkit-scrollbar-thumb:hover { background-color: #aaa; }
      .sub-option label { display:block; font-weight:normal; margin-bottom:2px; }
      
      #filter-panel {
        font-size: 15px;
        background-color: #f8f9fa;
        border-radius: 10px;
        padding: 11px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.1);
        padding-bottom: 4px;
      }
      
      #filter-panel label {
        font-weight: 600;
        color: #333;
      }
      
      /* 让下拉框和标题之间间距变小 */
      #filter-panel .shiny-input-container {
        margin-top: 4px !important;
        margin-bottom: 1px !important;
      }
      
      * 全局：去掉 fluidPage 默认 padding，左右更贴边 */
      .container-fluid {
        padding: 0 !important;
        margin: 0 !important;
        width: 100%;
        max-width: 100%;
      }
  
      /* sidebarPanel：固定窄宽度，更紧凑 */
      .col-sm-3 {
        max-width: 250px !important;
        padding-right: 8px !important;
        margin-right: 0 !important;
      }
      
      #sidebarLayout { 
        max-height: 350px !important;
      }
      
      /* “Select Ratings” 白框样式（与 Select Categories 一致） */
      #rating-section {
        background-color: #f8f9fa;
        border-radius: 10px;
        padding: 10px 12px;
        box-shadow: 0 2px 6px rgba(0,0,0,0.1);
        margin-bottom: 10px;
        font-weight: 600;
        color: #333;
      }
      
      /* 让下拉框和标题之间间距变小 */
      #rating-section .shiny-input-container {
        margin-top: 4px !important;
        margin-bottom: 4px !important;
      }
      
      /* 下拉框本身也稍微压紧 */
      #rating-section select {
        padding: 4px 8px;
        font-size: 14px;
        padding-bottom: 4px;
      }
      
      /* “Opening Status” 白框样式 */
      #open-filter { 
        background-color: #f8f9fa;
        border-radius: 10px;
        padding: 10px 12px;
        box-shadow: 0 2px 6px rgba(0,0,0,0.1);
        margin-bottom: 10px;
        font-weight: 600;
        color: #333;
        padding-bottom: 4px;
      }
      
      /* 让复选框与标题之间的间距更小 */
      #open-filter .shiny-input-container {
        margin-top: 4px !important;
        margin-bottom: 4px !important;
      }
      
      /* 复选框本身字体大小、行距调整 */
      #open-filter label {
        font-size: 14px;
        font-weight: 600;
        margin-bottom: 2px;
      }
      
      /* “Select Regional Range” 白框样式 */
      #regional-range {
        background-color: #f8f9fa;
        border-radius: 10px;
        padding: 10px 12px;
        box-shadow: 0 2px 6px rgba(0,0,0,0.1);
        margin-bottom: 4px;
        font-weight: 600;
        color: #333;
      }
      
      /* ----------- 让按钮区更整齐 ----------- */
      #regional-range .btn {
        border-radius: 8px !important;
        font-weight: 600;
        font-size: 14px;
        padding: 8px 0;
        width: 100%;
        text-align: center;
        transition: all 0.2s ease-in-out;
      }
      
      /* 主操作按钮：Locate Me（蓝色） */
      #locate_btn {
        background-color: #0d6efd;
        border: 1px solid #0b5ed7;
        color: #3478f6;
      }
      #locate_btn:hover {
        background-color: #0b5ed7;
      }
      
      /* 次操作按钮：Clear（灰红色） */
      #clear_btn {
        background-color: #fff;
        border: 1px solid #dc3545;
        color: #dc3545;
      }
      #clear_btn:hover {
        background-color: #dc3545;
        color: white;
      }
      
      /* 让两个按钮之间的距离更自然 */
      #regional-range .col-sm-6, 
      #regional-range .col-md-6 {
        padding-left: 4px;
        padding-right: 4px;
      }
      
      /* 下拉框和按钮之间多一点空间 */
      #regional-range .shiny-input-container {
        margin-bottom: 12px !important;
      }
  
      /* mainPanel：铺满剩余空间 */
      .col-sm-9 {
        flex: 1 1 auto;
        width: calc(100% - 250px);
        padding-left: 0 !important;
        padding-right: 0 !important;
        margin-left: 0 !important;
      }
  
      /* 地图容器：完全铺满右侧区域 */
      #map-container {
        width: 99.3% !important;
        height: calc(100vh - 110px) !important;
        margin: 0;
        padding: 0;
      }
  
      /* 让 leafletOutput 自适应容器 */
      #map {
        width: 100% !important;
        height: 100% !important;
      }
  
      /* 标题区域与主体之间距离稍微减小 */
      h2, .title {
        margin-bottom: 20px;
      }

      /* 发光动画（青蓝光）仅用于商铺，不用于定位图标 */
      .leaflet-marker-icon.active-glow {
        animation: glowPulse 1.5s ease-in-out infinite;
        z-index: 1000 !important;
      }
      @keyframes glowPulse {
        0%   { filter: brightness(1) drop-shadow(0 0 0 rgba(0,255,255,0)); }
        50%  { filter: brightness(1.3) drop-shadow(0 0 12px rgba(0,255,255,0.9)); }
        100% { filter: brightness(1) drop-shadow(0 0 0 rgba(0,255,255,0)); }
      }
      
      .checkbox label, .radio label {
        margin-left: 6px;
      }
      
      #locate_btn {
        font-size: 14px;
        font-weight: 500;
        padding: 8px 0;
        border-radius: 6px;
        border: 1px solid #ccc;
        background-color: #f9f9f9;
        transition: all 0.2s ease-in-out;
      }
      
      #clear_btn {
        font-size: 14px;
        font-weight: 500;
        padding: 8px 0;
        border-radius: 6px;
        border: 1px solid #ccc;
        background-color: #f9f9f9;
        transition: all 0.2s ease-in-out;
      }
      
      /* hover 效果 */
      #locate_btn:hover, #clear_btn:hover {
        background-color: #eaeaea;
        border-color: #aaa;
      }
  
      /* 按钮文字间距更宽，视觉平衡 */
      #locate_btn span, #clear_btn span {
        letter-spacing: 0.3px;
      }
  
      /* 让按钮在同一行更居中 */
      .shiny-input-container .btn {
        width: 100%;
        text-align: center;
      }
  
      /* 两个按钮间距 */
      #locate_btn { margin-right: 4px; }
      #clear_btn { margin-left: 4px; }
  
      /* Row 内水平对齐 */
      .shiny-row {
        display: flex;
        gap: 8px;
      }
      
      /* ===== 地图加载动画样式 ===== */
      #map-container {
        position: relative;
      }
    
      #loading-overlay {
        position: absolute;
        top: 0; left: 0;
        width: 100%; height: 100%;
        background-color: rgba(255,255,255,0.92);
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        z-index: 9999;
      }
    
      .spinner-border {
        width: 3rem; height: 3rem;
        border: 0.35em solid #2d9cdb;
        border-top: 0.35em solid transparent;
        border-radius: 50%;
        animation: spin 0.8s linear infinite;
      }
    
      @keyframes spin {
        0% { transform: rotate(0deg); }
        100% { transform: rotate(360deg); }
      }
    </style>

    <script>
      function sendSubSelection(type){
        let checked = [];
        $("input[name=sub_" + type + "]:checked").each(function(){ checked.push($(this).val()); });
        Shiny.setInputValue("sub_" + type, checked, {priority:"event"});
      }

      // All 勾中 -> 取消其它，收起子类
      $(document).on("change", "#main_all", function(){
        if($(this).is(":checked")){
          ["restaurant","bar","cafe","milktea"].forEach(function(t){
            $("#main_" + t).prop("checked", false);
            $("#sub_" + t).slideUp(0);
            $("input[name=sub_" + t + "]").prop("checked", false);
            sendSubSelection(t);
          });
        }
      });

      // 主类互斥 + 展开/收起子类
      $(document).on("change", "input[id^=main_]:not(#main_all)", function(){
        if($("#main_all").is(":checked")) $("#main_all").prop("checked", false).trigger("change");
        const type = $(this).attr("id").replace("main_","");
        if($(this).is(":checked")) { $("#sub_" + type).slideDown(200); }
        else {
          $("#sub_" + type).slideUp(200);
          $("input[name=sub_" + type + "]").prop("checked", false);
          sendSubSelection(type);
        }
      });

      $(document).on("change", "input[name^=sub_]", function(){
        const group=$(this).attr("name").replace("sub_","");
        sendSubSelection(group);
      });

      // 商铺 marker 发光（排除定位图标）
      let lastActive = null;
      document.addEventListener("click", function(e){
        const el = e.target.closest(".leaflet-marker-icon");
        if(el && !el.classList.contains("user-location")){
          if(lastActive && lastActive!==el) lastActive.classList.remove("active-glow");
          el.classList.toggle("active-glow");
          lastActive = el.classList.contains("active-glow") ? el : null;
        } else if(!el){
          setTimeout(()=>{
            if(lastActive){
              lastActive.classList.remove("active-glow");
              lastActive=null;
            }
          },100);
        }
      });

      // 📍 Locate Me（高精度 + 不缓存）
      $(document).on("click", "#locate_btn", function(){
        if(navigator.geolocation){
          navigator.geolocation.getCurrentPosition(function(pos){
            Shiny.setInputValue("user_location", 
              {lat:pos.coords.latitude, lon:pos.coords.longitude, ts: Date.now()}, {priority:"event"});
          }, function(err){
            alert("Unable to get location: " + err.message);
          }, {enableHighAccuracy: true, maximumAge: 0, timeout: 10000});
        } else {
          alert("Geolocation not supported in this browser.");
        }
      });
    </script>
    ')
    ,
    tags$style(HTML("
      #mainApp .container-fluid {
        padding-left: 15px !important;
        padding-right: 15px !important;
        margin-left: auto !important;
        margin-right: auto !important;
      }
      #mainApp .col-sm-3 {
        max-width: none !important;
        padding-right: 15px !important;
      }
      #mainApp .col-sm-9 {
        width: auto !important;
        padding-left: 15px !important;
        padding-right: 15px !important;
      }
      #map-tab .col-sm-3 {
        max-width: 250px !important;
        padding-right: 8px !important;
        margin-right: 0 !important;
      }
      #map-tab .col-sm-9 {
        width: calc(100% - 250px) !important;
        padding-left: 0 !important;
        padding-right: 0 !important;
        margin-left: 0 !important;
      }
    "))
  ),
  
  
  # Welcome 区
  div(
    id="welcome",
    div(
      class="glass",
      h2(HTML("<span class='welcome-title'>Flavours of Melbourne City</span>")),
      div(
        style="flex:1;width:100%;display:flex;justify-content:center;align-items:center;background:rgba(255,255,255,0.08);border-radius:20px;padding:20px;box-sizing:border-box;",
        HTML("
          <iframe 
            src='https://public.tableau.com/views/Word_17615465535280/Dashboard1?:showVizHome=no&:embed=true&:toolbar=no&:render=false&:device=desktop'
            width='100%' height='100%'
            style='border:none;background:transparent;border-radius:16px;'
            allowtransparency='true'>
          </iframe>
        ")
      ),
      tags$button('Start to Explore', class='enter-btn', onclick='goToMain();')
    )
  ),
  
  # ------------------ 主页面：NavbarPage ------------------
  div(
    id="mainApp",
    navbarPage(
      title = "",
      id = "navtabs",
      
      tabPanel(
        "Map",
        div(
          id = "map-tab",
          titlePanel("🍽️ Melbourne CBD Food & Drink Map"),
          sidebarLayout(
            sidebarPanel(
              width = 3,
              tags$div(
                id = "filter-panel",
                tags$label("Select Categories: 🍴"),
                tags$div(
                  class = "main-block",
                  checkboxInput("main_all", "All", value = TRUE)
                ),
                tags$div(
                  class = "main-block",
                  checkboxInput("main_restaurant", "Restaurant", value = FALSE),
                  tags$div(id = "sub_restaurant", class = "sub-option")
                ),
                tags$div(
                  class = "main-block",
                  checkboxInput("main_bar", "Bar", value = FALSE),
                  tags$div(id = "sub_bar", class = "sub-option")
                ),
                tags$div(
                  class = "main-block",
                  checkboxInput("main_cafe", "Cafe", value = FALSE),
                  tags$div(id = "sub_cafe", class = "sub-option")
                ),
                tags$div(
                  class = "main-block",
                  checkboxInput("main_milktea", "Milktea", value = FALSE),
                  tags$div(id = "sub_milktea", class = "sub-option")
                ),
                tags$div(id = "choose_hint", style = "display:none; color:#777; margin-top:6px;")
              ),
              br(),
              div(
                id = "rating-section",
                tags$label("Select Rating: ⭐"),
                selectInput(
                  "rating_filter",
                  NULL,
                  choices = c(
                    "All Ratings" = "all",
                    "4.5 +" = "4.5_up",
                    "4.0 - 4.5" = "4.0_4.5",
                    "3.5 - 4.0" = "3.5_4.0",
                    "3.0 - 3.5" = "3.0_3.5",
                    "3.0 -" = "below_3"
                  ),
                  selected = "all",
                  width = "150px"
                )
              ),
              div(
                id = "open-filter",
                tags$label("Opening Status: 🟢"),
                checkboxInput(
                  "show_open_now",
                  "Show Open",
                  value = FALSE
                )
              ),
              div(
                id = "regional-range",
                selectInput(
                  "radius_select",
                  "Select Regional Range:",
                  choices = c("500m" = 500, "1km" = 1000, "2km" = 2000),
                  selected = 500,
                  width = "160px"
                ),
                fluidRow(
                  column(6, actionButton("locate_btn", "Locate Me", width = "100%")),
                  column(6, actionButton("clear_btn", "Clear", width = "95%"))
                )
              )
            ),
            mainPanel(
              width = 9,
              div(
                id = "map-container",
                leafletOutput("map", height = 650)
              )
            )
          )
        )
      ),
      
      tabPanel(
        "Route",
        route_module_ui("route")
      ),
      
      tabPanel(
        "Detail",
        
        # 搜索框
        fluidRow(
          column(
            12,
            selectizeInput(
              "pick", label = NULL,
              choices = c("", sort(unique(stats::na.omit(all$name)))),
              selected = "",
              options  = list(placeholder = "search keyword…", create = FALSE),
              width    = "100%"
            )
          )
        ),
        
        # 动态主内容：要么全屏 skyline，要么整页 detail 布局
        uiOutput("mainContent")
      ),
      
      
      # ====== 你的原始搜索页 UI（原样放回）======
      
      
      
    )
  )
)

# ----------------- SERVER -----------------
server <- function(input, output, session){
  
  route_module_server("route")
  
  # ---------- 静态资源里的 skyline 图片（www/melbourne-skyline.jpg） ----------
  skyline_file <- "sky-line.jpg"
  if (!file.exists(file.path("www", skyline_file))) {
    skyline_file <- NULL  # 若未放入 www，用一个在线兜底图
  }
  
  # ---------- 状态 ----------
  last_pick    <- reactiveVal(NULL)  # 记录最后一次有效选择
  has_selected <- reactiveVal(FALSE) # 是否曾经选中过店铺
  show_skyline <- reactiveVal(TRUE)  # 是否显示全屏 skyline（初始 TRUE）
  user_loc     <- reactiveVal(NULL)  # Map 页定位状态
  
  # 监听搜索框变化
  observeEvent(input$pick, ignoreInit = TRUE, {
    if (!is.null(input$pick) && nzchar(input$pick)) {
      # 有有效选择
      last_pick(input$pick)
      has_selected(TRUE)
      show_skyline(FALSE)   # 选中过后，显示 detail
    } else {
      # 被清空：保持 detail，不显示 skyline
      # （如果想回填最后选项，可解注下面）
      # updateSelectizeInput(session, "pick", selected = last_pick(), server = TRUE)
    }
  })
  
  # ✅ 改动点：无论选中过没选过，从 Map/Route 切回 Detail 都显示 skyline
  observeEvent(input$navtabs, ignoreInit = TRUE, {
    if (identical(input$navtabs, "Detail")) {
      show_skyline(TRUE)
    }
  })
  
  # 选中记录（为空时走 last_pick）
  sel <- reactive({
    nm <- if (!is.null(input$pick) && nzchar(input$pick)) input$pick else last_pick()
    req(!is.null(nm) && nzchar(nm))
    rec <- all %>% filter(name == nm) %>% slice(1)
    req(nrow(rec) == 1)
    rec
  })
  
  # ================= 主内容：全屏 skyline 或 详情布局 =================
  output$mainContent <- renderUI({
    if (isTRUE(show_skyline())) {
      bg_img <- if (!is.null(skyline_file)) skyline_file else
        "https://upload.wikimedia.org/wikipedia/commons/b/bc/Melbourne_skyline_sunset.jpg"
      
      # 全屏欢迎图
      return(
        div(
          style = sprintf("
            position:fixed;
            inset:0;
            z-index:0;
            background:url('%s') center center/cover no-repeat;
          ", bg_img),
          div(
            style = "
              position:absolute;
              inset:0;
              background:linear-gradient(to bottom right, rgba(0,0,0,0.35), rgba(0,0,0,0.55));
            "
          ),
          div(
            style = "
              position:absolute;
              top:50%;
              left:50%;
              transform:translate(-50%, -50%);
              color:white;
              text-align:center;
              font-family:'Poppins', sans-serif;
              animation:fadeIn 1.4s ease;
            ",
            h1("Discover Melbourne’s Best Food & Drinks",
               style="font-size:46px;font-weight:700;line-height:1.3;max-width:900px;")
          ),
          tags$style(HTML("
            @keyframes fadeIn {
              from {opacity:0; transform:translate(-50%, -48%);}
              to   {opacity:1; transform:translate(-50%, -50%);}
            }
          "))
        )
      )
    }
    
    # ======= Detail 详情布局 =======
    tagList(
      fluidRow(
        column(
          width = 6,
          div(class = "hero-card",
              uiOutput("shopImage"),
              br(),
              div(class = "meta-card", htmlOutput("linkPhoneAddress"))
          ),
          br(),
          div(class = "kpi-card",
              uiOutput("ratingProgress"),
              br(),
              uiOutput("kpisText")
          ),
          br(),
          div(class = "desc-card", uiOutput("shopDesc"))
        ),
        
        column(
          width = 6,
          div(class = "hero-card",
              h4("Where are its peers nearby?"),
              div(id = "peerMapWrapper", leafletOutput("peerMap", height = 360)),
              tags$small(class = "muted", id = "peerCaption")
          ),
          br(),
          div(class = "hero-card",
              h4("Opening hours (local)"),
              uiOutput("openNowRight"),
              plotOutput("openHoursPlot", height = 220)
          )
        )
      )
    )
  })
  
  # =================== 下面保持你的原逻辑 ===================
  
  output$shopImage <- renderUI({
    # ⚠️ 已移除 “当 pick 为空显示 skyline”的逻辑，只负责轮播图
    s <- sel()
    pid <- scalar_chr(s$place_id)
    src_dir <- scalar_chr(s$src)
    imgs <- character(0)
    if (nzchar(pid) && nzchar(src_dir)) {
      fs_dir <- file.path(src_dir, pid)
      if (dir.exists(fs_dir)) {
        files <- list.files(fs_dir, pattern="(?i)\\.(jpg|jpeg|png|gif|webp)$", full.names = FALSE)
        if (length(files)) {
          ord <- order(suppressWarnings(as.numeric(gsub("\\D+","",files))), files, na.last = TRUE)
          files <- unique(files[ord])
        }
        imgs <- file.path(src_dir, pid, files)
      }
    }
    if (length(imgs) == 0) {
      return(div(class="shop-img", style="display:flex;align-items:center;justify-content:center;color:#aaa;", "No image"))
    }
    cid <- paste0("carousel_", as.integer(runif(1,1,1e9))); n <- length(imgs)
    items <- lapply(seq_along(imgs), function(i){
      classes <- if (i == 1) "carousel-item active" else "carousel-item"
      counter <- div(style="position:absolute; right:10px; top:10px; background:rgba(0,0,0,0.45); color:#fff; font-size:12px; padding:2px 6px; border-radius:999px; z-index:5;", paste(i,"/",n))
      div(class=classes,
          div(style="position:relative;",
              counter,
              img(src=imgs[[i]], class="d-block w-100 shop-img",
                  onerror="this.onerror=null; this.src='data:image/svg+xml;utf8,<svg xmlns=%22http://www.w3.org/2000/svg%22 width=%22640%22 height=%22320%22><rect width=%22100%25%22 height=%22100%25%22 fill=%22%23f2f2f2%22/><text x=%2250%25%22 y=%2250%25%22 font-size=%2220%22 text-anchor=%22middle%22 fill=%22%23999%22 dy=%22.35em%22>No image</text></svg>';")))
    })
    indicators <- if (n>1) lapply(seq_len(n), function(i){
      tags$button(type="button", `data-bs-target`=paste0("#",cid), `data-bs-slide-to`=i-1,
                  class=if (i==1) "active" else NULL, `aria-current`=if (i==1) "true" else NULL,
                  `aria-label`=paste("Slide",i))
    })
    div(id=cid, class="carousel slide", `data-bs-ride`=if (n>1) "carousel" else NULL,
        `data-bs-interval`=if (n>1) "4000" else NULL, `data-bs-touch`="true",
        if (!is.null(indicators)) div(class="carousel-indicators", indicators),
        div(class="carousel-inner", items),
        if (n>1) tagList(
          tags$button(class="carousel-control-prev", type="button",
                      `data-bs-target`=paste0("#",cid), `data-bs-slide`="prev",
                      tags$span(class="carousel-control-prev-icon", `aria-hidden`="true"),
                      tags$span(class="visually-hidden","Previous")),
          tags$button(class="carousel-control-next", type="button",
                      `data-bs-target`=paste0("#",cid), `data-bs-slide`="next",
                      tags$span(class="carousel-control-next-icon", `aria-hidden`="true"),
                      tags$span(class="visually-hidden","Next"))
        )
    )
  })
  
  output$linkPhoneAddress <- renderText({
    s <- sel()
    website <- scalar_chr(s$website)
    phone_v <- scalar_chr(s$phone)
    addr_v  <- scalar_chr(s$address)
    link  <- if (nzchar(website)) sprintf("<a href='%s' target='_blank'>Website ↗</a>", htmlEscape(website)) else "<span class='muted'>No Website Info</span>"
    phone <- if (nzchar(phone_v)) htmlEscape(phone_v) else "<span class='muted'>No Phone Info</span>"
    addr  <- if (nzchar(addr_v)) htmlEscape(addr_v) else "<span class='muted'>No Address Info</span>"
    sprintf("<div><div>%s</div><div class='muted'>%s</div><div class='muted'>%s</div></div>", link, phone, addr)
  })
  
  output$shopDesc <- renderUI({
    s <- sel(); txt <- scalar_chr(s$description, "No Description Info")
    HTML(htmlEscape(txt))
  })
  
  output$kpisText <- renderUI({
    s <- sel()
    lvl_val <- scalar_value(s$price_level)
    pr_txt <- price_range(lvl_val)
    pr_txt <- scalar_chr(pr_txt, "No Price Info")
    catg   <- scalar_chr(s$category, "N/A")
    HTML(paste0(
      "<div style='height:10px'></div>",
      "<div><span class='kpi'>Price range: ", htmlEscape(pr_txt), "</span></div>",
      "<div style='height:10px'></div>",
      "<div><span class='kpi'>Category: ", htmlEscape(catg), "</span></div>"
    ))
  })
  
  output$ratingProgress <- renderUI({
    s <- sel()
    sc  <- suppressWarnings(as.numeric(s$rating)); sc <- ifelse(is.na(sc), 0, sc)
    pct <- max(0, min(100, round(100 * sc / 5, 1))); uid <- paste0("rb_", as.integer(runif(1, 1, 1e9)))
    HTML(sprintf('
      <div class="rating-wrap">
        <div class="rating-label">Rating: <span>%s</span> / 5</div>
        <div id="%s_wrap" style="position:relative; width:25%%;">
          <div id="%s_outer" class="progress-outer" style="overflow:hidden;">
            <div id="%s_inner" class="progress-inner"></div>
          </div>
          <div id="%s_rating_marker"
               style="position:absolute; top:22px; font-size:12px; color:#444; left:0; transform:translateX(-50%%);">%s</div>
        </div>
      </div>
      <script>
        setTimeout(function(){
          var inner  = document.getElementById("%s_inner");
          var rmark  = document.getElementById("%s_rating_marker");
          if (inner && rmark){
            inner.style.width = "%s%%";
            rmark.style.transition = "left 800ms ease";
            var p = %s; if (p < 4) p = 4; if (p > 98) p = 98;
            rmark.style.left = p + "%%";
          }
        }, 75);
      </script>
    ', htmlEscape(sprintf("%.1f", sc)),
                 uid, uid, uid, uid, htmlEscape(sprintf("%.1f", sc)),
                 uid, uid, pct, pct))
  })
  
  hav_dist_m <- function(lat1, lon1, lat2, lon2){
    rad <- pi/180; dlat <- (lat2-lat1)*rad; dlon <- (lon2-lon1)*rad
    a <- sin(dlat/2)^2 + cos(lat1*rad)*cos(lat2*rad)*sin(dlon/2)^2
    2 * 6371000 * asin(pmin(1, sqrt(a)))
  }
  RADIUS_M <- 1000
  
  output$peerMap <- renderLeaflet({
    s <- sel()
    lat0 <- as.numeric(scalar_value(s$lat))
    lon0 <- as.numeric(scalar_value(s$lon))
    shiny::validate(shiny::need(!is.na(lat0) && !is.na(lon0), "No coordinates for this venue."))
    
    src_key <- scalar_chr(s$src)
    peer <- all %>%
      filter(src == src_key) %>%
      filter(!is.na(lat) & !is.na(lon))
    
    d <- hav_dist_m(lat0, lon0, peer$lat, peer$lon)
    n_in <- sum(!is.na(d) & d <= RADIUS_M, na.rm = TRUE) - 1L
    n_in <- max(0L, n_in)
    
    icon_file <- switch(
      src_key,
      "restaurant"         = "Restaurant_icon.png",
      "bars"               = "Bar_icon.png",
      "cafe_brunch_bakery" = "Cafe_icon.png",
      "milktea_juice"      = "Milktea_icon.png",
      NULL
    )
    if (!is.null(icon_file) && file.exists(file.path("www", icon_file))) {
      icon_selected <- makeIcon(
        iconUrl = icon_file, iconWidth = 48, iconHeight = NULL, iconAnchorX = 24, iconAnchorY = 48
      )
    } else {
      icon_selected <- makeAwesomeIcon(icon = "map-marker-alt", markerColor = "blue", iconColor = "white")
    }
    
    disp <- SRC_DISPLAY[[src_key]] %||% toupper(gsub("_", " ", src_key))
    disp <- scalar_chr(disp, "")
    
    leaflet(options = leafletOptions(zoomControl = TRUE, preferCanvas = TRUE)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = lon0, lat = lat0, zoom = 15) %>%
      addMapPane("radius", zIndex = 420) %>%
      addMapPane("peers",  zIndex = 430) %>%
      addCircles(
        lng = lon0, lat = lat0, radius = RADIUS_M,
        color = "#3478f6", weight = 1, opacity = 0.7, fillOpacity = 0.05,
        options = pathOptions(pane = "radius")
      ) %>%
      addCircleMarkers(
        data = peer, lng = ~lon, lat = ~lat,
        radius = 6, stroke = FALSE, fillOpacity = 0.75, fillColor = "#AFDBC2",
        label = ~name,
        labelOptions = labelOptions(direction = "auto", opacity = 0.95, textsize = "13px", sticky = TRUE),
        options = pathOptions(pane = "peers")
      ) %>%
      addMarkers(
        lng = lon0, lat = lat0, icon = icon_selected,
        label = paste0(scalar_chr(s$name), " (", disp, ")"),
        labelOptions = labelOptions(direction = "top", textsize = "13px", opacity = 0.95)
      ) %>%
      {
        session$sendCustomMessage(
          "peerCaptionText",
          list(text = sprintf("Within %dm: %d nearby %s.", RADIUS_M, n_in, disp))
        )
        .
      }
  })
  
  output$openNowRight <- renderUI({
    s <- sel()
    df  <- parse_opening_hours(as.character(s$openinghours %||% ""))
    now <- with_tz(Sys.time(), "Australia/Melbourne")
    open_flag <- is_open_at(df, now)
    info <- today_hours_and_next_open(df, now)
    
    badge <- if (open_flag)
      tags$span(style="display:inline-block;background:#28a745;color:#fff;border-radius:999px;padding:2px 10px;font-weight:600;margin-right:10px;", "Open now")
    else
      tags$span(style="display:inline-block;background:#dc3545;color:#fff;border-radius:999px;padding:2px 10px;font-weight:600;margin-right:10px;", "Closed")
    
    div(
      badge,
      if (!open_flag && nzchar(info$next_open))
        tags$div(class="muted", paste("Next open:", info$next_open))
    )
  })
  
  output$openHoursPlot <- renderPlot({
    s  <- sel()
    df <- parse_opening_hours(as.character(s$openinghours %||% ""))
    req(nrow(df) > 0)
    
    seg <- expand_overnight_for_plot(df)
    lvl <- c("MON","TUE","WED","THU","FRI","SAT","SUN")
    seg$day <- toupper(seg$day)
    seg <- seg[seg$day %in% lvl, ]
    seg$day <- factor(seg$day, levels = lvl, ordered = TRUE)
    
    now_hr <- hour(with_tz(Sys.time(), "Australia/Melbourne")) +
      minute(with_tz(Sys.time(), "Australia/Melbourne"))/60
    
    ggplot(seg) +
      geom_segment(aes(x = start_num, xend = end_num, y = day, yend = day),
                   linewidth = 4, lineend = "round", color = "#4E79A7") +
      geom_vline(xintercept = now_hr, color = "#E15759",
                 linetype = "dashed", linewidth = 0.6) +
      geom_text(
        data = data.frame(x = now_hr, y = lvl[1], lab = "NOW"),
        mapping = aes(x = x, y = y, label = lab),
        inherit.aes = FALSE,
        color = "#E15759", vjust = -1.2, fontface = "bold", size = 3.5
      ) +
      scale_x_continuous(limits = c(0, 24), breaks = seq(0, 24, 4),
                         labels = paste0(seq(0, 24, 4), ":00")) +
      scale_y_discrete(limits = rev(lvl), expand = expansion(add = c(0.05, 0.35))) +
      coord_cartesian(clip = "off") +
      theme_minimal(base_size = 11) +
      labs(x = "Time of Day", y = NULL, title = "Weekly Opening Hours") +
      theme(plot.title = element_text(size=12, face="bold", hjust=0.5),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "#F3F3F3", colour = NA),
            plot.background = element_rect(fill = "#F3F3F3", colour = NA),
            plot.margin = margin(t = 16, r = 8, b = 8, l = 8))
  })

  # ---------------- Map 页逻辑 ----------------
  observe({
    insert_subs <- function(type, container_id) {
      subs <- sort(unique(places$category[places$type == type]))
      if (length(subs) == 0) return()
      type_lower <- tolower(type)
      labels <- vapply(subs, function(s) {
        sprintf("<label><input type='checkbox' name='sub_%s' value='%s'/> %s</label>",
                type_lower, htmlEscape(s), htmlEscape(s))
      }, character(1))
      html <- paste0("<div class='sub-option-inner'>", paste0(labels, collapse=""), "</div>")
      js <- sprintf("$('#%s').html(%s);", container_id, jsonlite::toJSON(html, auto_unbox = TRUE))
      shinyjs::runjs(js)
    }
    insert_subs("Restaurant", "sub_restaurant")
    insert_subs("Bar", "sub_bar")
    insert_subs("Cafe", "sub_cafe")
    insert_subs("Milktea", "sub_milktea")
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stadia.AlidadeSmooth, group = "Light (Modern)") %>%
      addProviderTiles(providers$Stadia.AlidadeSmoothDark, group = "Dark (Modern)") %>%
      addProviderTiles(providers$CartoDB.Voyager, group = "Voyager") %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Minimal Gray") %>%
      setView(lng = 144.9631, lat = -37.8100, zoom = 15) %>%
      addLayersControl(
        baseGroups = c("Light (Modern)", "Dark (Modern)", "Voyager", "Minimal Gray"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addEasyButton(
        easyButton(
          icon = "fa-bullseye",
          title = "Back to Melbourne CBD",
          onClick = JS("function(btn, map){ map.setView([-37.8100, 144.9631], 15); }")
        )
      )
  })
  
  get_filtered_df <- reactive({
    selected_main <- c()
    if (isTRUE(input$main_all) ||
        (!isTRUE(input$main_restaurant) && !isTRUE(input$main_bar) &&
         !isTRUE(input$main_cafe) && !isTRUE(input$main_milktea))) {
      selected_main <- c("Restaurant", "Bar", "Cafe", "Milktea")
    } else {
      if (isTRUE(input$main_restaurant)) selected_main <- c(selected_main, "Restaurant")
      if (isTRUE(input$main_bar))        selected_main <- c(selected_main, "Bar")
      if (isTRUE(input$main_cafe))       selected_main <- c(selected_main, "Cafe")
      if (isTRUE(input$main_milktea))    selected_main <- c(selected_main, "Milktea")
    }
    df <- places %>% filter(type %in% selected_main)
    
    for (t in c("Restaurant", "Bar", "Cafe", "Milktea")) {
      sub_vals <- input[[paste0("sub_", tolower(t))]]
      if (!is.null(sub_vals) && length(sub_vals) > 0) {
        df <- df %>% filter(!(type == t) | (category %in% sub_vals))
      }
    }
    
    loc <- user_loc()
    r <- as.numeric(input$radius_select)
    if (!is.null(loc) && !is.na(r) && r > 0) {
      df$dist <- geosphere::distHaversine(cbind(df$lon, df$lat), c(loc$lon, loc$lat))
      df <- df[df$dist <= r, , drop = FALSE]
    }
    
    df$open_now <- sapply(df$openinghour, is_open_today_now)
    
    if (!is.null(input$rating_filter) && input$rating_filter != "all") {
      df <- df %>%
        filter(!is.na(rating)) %>%
        dplyr::filter(
          (input$rating_filter == "4.5_up"   & rating >= 4.5) |
            (input$rating_filter == "4.0_4.5"  & rating >= 4.0 & rating < 4.5) |
            (input$rating_filter == "3.5_4.0"  & rating >= 3.5 & rating < 4.0) |
            (input$rating_filter == "3.0_3.5"  & rating >= 3.0 & rating < 3.5) |
            (input$rating_filter == "below_3"  & rating < 3.0)
        )
    }
    
    if (isTRUE(input$show_open_now)) {
      df <- df[df$open_now == TRUE, , drop = FALSE]
    }
    
    df
  })
  
  observe({
    df <- get_filtered_df()
    if (nrow(df) == 0) {
      leafletProxy("map", session = session) %>% clearGroup("poi_markers")
      return()
    }
    df$open_now <- sapply(df$openinghour, is_open_today_now)
    df$icon_file <- ifelse(df$open_now, paste0(df$type, "_icon.png"), paste0(df$type, "_icon_gray.png"))
    
    icons_set <- icons(
      iconUrl = df$icon_file,
      iconWidth = 40,
      iconHeight = 55,
      iconAnchorX = 20,
      iconAnchorY = 55,
      popupAnchorX = 1,
      popupAnchorY = -55
    )
    
    popup_html <- function(row) {
      clean_address <- gsub(",\\s*Australia\\s*$", "", row$address, ignore.case = TRUE)
      rating_html <- if (!is.na(row$rating) && row$rating != "") {
        sprintf("⭐ <b>%.1f</b>", as.numeric(row$rating))
      } else {
        "<i>No Rating Info</i>"
      }
      phone_html <- if (!is.na(row$phone) && row$phone != "") {
        sprintf("📞 %s", htmlEscape(row$phone))
      } else {
        "<i>No Phone Info</i>"
      }
      website_html <- if (!is.na(row$website) && row$website != "") {
        sprintf('<a href="%s" target="_blank">🔗 Visit Website</a>', htmlEscape(row$website))
      } else {
        "<i>No Website Info</i>"
      }
      link <- sprintf(
        '<a href="https://www.google.com/maps/dir/?api=1&destination=%f,%f" target="_blank">🚗 Map</a>',
        row$lat,
        row$lon
      )
      sprintf(
        "<b>%s</b><br/>%s<br/>%s<br/>%s<br/>%s<br/>%s<br/>%s",
        htmlEscape(row$name),
        htmlEscape(clean_address),
        rating_html,
        if (row$open_now) "🟢 <i>Open</i>" else "🔴 <i>Closed</i>",
        phone_html,
        website_html,
        link
      )
    }
    
    leafletProxy("map", session = session) %>%
      clearGroup("poi_markers") %>%
      addMarkers(
        lng = df$lon,
        lat = df$lat,
        icon = icons_set,
        popup = lapply(seq_len(nrow(df)), function(i) popup_html(df[i, ])),
        options = markerOptions(className = "poi-marker"),
        clusterOptions = markerClusterOptions(),
        group = "poi_markers"
      )
  })
  
  observeEvent(input$user_location, {
    loc <- input$user_location
    if (is.null(loc$lat) || is.null(loc$lon)) return()
    user_loc(loc)
    r <- as.numeric(input$radius_select)
    
    leafletProxy("map", session = session) %>%
      clearGroup("user_marker") %>%
      clearGroup("range_circle") %>%
      addMarkers(
        lng = loc$lon,
        lat = loc$lat,
        icon = icons(
          iconUrl = "https://unpkg.com/leaflet@1.9.3/dist/images/marker-icon.png",
          iconWidth = 25,
          iconHeight = 41,
          iconAnchorX = 12,
          iconAnchorY = 20
        ),
        label = "You are here 📍",
        options = markerOptions(className = "user-location", clickable = FALSE),
        group = "user_marker"
      ) %>%
      addCircles(
        lng = loc$lon,
        lat = loc$lat,
        radius = r,
        color = "#3478f6",
        fillColor = "#9EC5FE",
        fillOpacity = 0.3,
        group = "range_circle"
      ) %>%
      flyTo(lng = loc$lon, lat = loc$lat, zoom = 15)
  })
  
  observeEvent(input$radius_select, {
    loc <- user_loc()
    if (is.null(loc)) return()
    r <- as.numeric(input$radius_select)
    leafletProxy("map", session = session) %>%
      clearGroup("range_circle") %>%
      addCircles(
        lng = loc$lon,
        lat = loc$lat,
        radius = r,
        color = "blue",
        fillColor = "skyblue",
        fillOpacity = 0.3,
        group = "range_circle"
      )
  })
  
  observeEvent(input$clear_btn, {
    user_loc(NULL)
    leafletProxy("map", session = session) %>%
      clearGroup("user_marker") %>%
      clearGroup("range_circle") %>%
      setView(lng = 144.9631, lat = -37.8100, zoom = 15)
  })
  
  session$onFlushed(function(){
    session$sendCustomMessage("peerCaptionText", list(text = ""))
  }, once = TRUE)
}



shinyApp(ui, server, options = list(launch.browser = TRUE)) 
