
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

# 如果有 tableau 脚本就加载（没有也不会报错）
if (file.exists("tableau-in-shiny-v1.2.R")) {
  source("tableau-in-shiny-v1.2.R")
}

# ----------------- 基础配置 & 数据 -----------------
POSTCODE_FILTER <- NA_character_
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
  theme = bs_theme(bootswatch = "journal"),
  tags$head(
    if (exists("setUpTableauInShiny")) setUpTableauInShiny(),
    
    # ✅ 恢复你原来的整段样式（含 .shop-img 高度等关键规则）
    tags$style(HTML("
      body {
        font-family: 'Inter', sans-serif;
        margin:0; padding:0;
        overflow-x:hidden;
        background: linear-gradient(135deg,#cfe2ff,#dbeafe,#f8fafc);
        
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
      .enter-btn { margin-top:24px; padding:12px 26px; font-size:18px; background:#3478f6; color:white; border:none; border-radius:10px; cursor:pointer; box-shadow:0 4px 10px rgba(52,120,246,0.25); transition:all .3s; }
      .enter-btn:hover { background:#265ed2; box-shadow:0 6px 14px rgba(52,120,246,0.35); }
      @import url('https://fonts.googleapis.com/css2?family=Playfair+Display:wght@700&family=Inter:wght@400;600&display=swap');
      .welcome-title { font-family:'Playfair Display',serif; font-size:52px; font-weight:700; letter-spacing:1px; text-align:center; background: linear-gradient(90deg,#4f46e5,#2563eb,#0ea5e9); -webkit-background-clip:text; -webkit-text-fill-color:transparent; margin-bottom:24px; }
      
      .hero-card {background:#fff; border-radius:16px; box-shadow:0 6px 20px rgba(0,0,0,0.07); padding:16px;}
      .meta-card {background:#fff; border-radius:12px; box-shadow:0 4px 16px rgba(0,0,0,0.06); padding:12px;}
      .desc-card {background:#fff; border-radius:12px; box-shadow:0 4px 16px rgba(0,0,0,0.06); padding:16px;}
      .kpi-card  {background:#fff; border-radius:12px; box-shadow:0 4px 16px rgba(0,0,0,0.06); padding:14px;}
      .shop-img  {width:100%; height:320px; object-fit:cover; border-radius:12px; background:#f2f2f2;}
      .kpi {font-weight:600; margin-right:16px;}
      .muted {color:#666;}
      .rating-wrap {display:flex; flex-direction:column; gap:6px;}
      .rating-label {font-weight:600;}
      .progress-outer {width:100%; height:16px; border-radius:999px; background:#f0f2f5; overflow:hidden;}
      .progress-inner {height:100%; width:0%; border-radius:999px; background:linear-gradient(90deg, #6ba8ff, #3478f6); transition:width 900ms ease;}
    ")),
    tags$style(HTML("
        @import url('https://fonts.googleapis.com/css2?family=Sorts+Mill+Goudy:wght@400;700&family=Poppins:wght@500;600&display=swap');

        .welcome-title {
          font-family: 'Sorts Mill Goudy', serif !important;
          font-weight: 700 !important;
          letter-spacing: 0.5px;
          background: linear-gradient(90deg,#3b82f6,#2563eb,#0ea5e9);
          -webkit-background-clip: text;
          -webkit-text-fill-color: transparent;
        }

        .navbar-brand {
          font-family: 'Sorts Mill Goudy', sans-serif !important;
          font-weight: 500 !important;
        }

        .navbar-nav > li > a,
        .navbar-nav .nav-link,
        .nav-tabs > li > a,
        .nav-tabs .nav-link {
          font-family: 'Arial', sans-serif !important;
          font-weight: 500 !important;
        }

        /* 各页区块标题 (h3, h4) */
        h3 {
          font-family: 'Arial', serif !important;
          font-weight: 700 !important;
          color: #2c3e50;
        }
        h4 {
          font-family: 'Arial', sans-serif !important;
          font-weight: 600 !important;
          color: #34495e;
        }
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
        h3("🗺️ Map Page"),
        p("This page is currently empty. You can add map content later.")
      ),
      
      tabPanel(
        "Route",
        h3("🚗 Route Page"),
        p("Future route planning content goes here.")
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
  
  # ---------- 静态资源里的 skyline 图片（www/melbourne-skyline.jpg） ----------
  skyline_file <- "sky-line.jpg"
  if (!file.exists(file.path("www", skyline_file))) {
    skyline_file <- NULL  # 若未放入 www，用一个在线兜底图
  }
  
  # ---------- 状态 ----------
  last_pick    <- reactiveVal(NULL)  # 记录最后一次有效选择
  has_selected <- reactiveVal(FALSE) # 是否曾经选中过店铺
  show_skyline <- reactiveVal(TRUE)  # 是否显示全屏 skyline（初始 TRUE）
  
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
    validate(need(!is.null(nm) && nzchar(nm), ""))
    all %>% filter(name == nm) %>% slice(1)
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
              font-family:'Sorts Mill Goudy', serif;
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
    s <- sel(); pid <- s$place_id %||% ""; src_dir <- s$src %||% ""
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
    link  <- if (nzchar(s$website %||% "")) sprintf("<a href='%s' target='_blank'>Website ↗</a>", s$website) else "<span class='muted'>No Website Info</span>"
    phone <- if (nzchar(s$phone %||% "")) s$phone else "<span class='muted'>No Phone Info</span>"
    addr  <- if (nzchar(s$address %||% "")) s$address else "<span class='muted'>No Address Info</span>"
    sprintf("<div><div>%s</div><div class='muted'>%s</div><div class='muted'>%s</div></div>", link, phone, addr)
  })
  
  output$shopDesc <- renderUI({
    s <- sel(); txt <- s$description %||% "No Description Info"
    HTML(htmlEscape(txt))
  })
  
  output$kpisText <- renderUI({
    s <- sel(); pr_txt <- price_range(s$price_level); catg <- s$category %||% "N/A"
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
    validate(need(!is.na(s$lat) && !is.na(s$lon), "No coordinates for this venue."))
    
    peer <- all %>%
      filter(src == s$src) %>%
      filter(!is.na(lat) & !is.na(lon))
    
    d <- hav_dist_m(s$lat, s$lon, peer$lat, peer$lon)
    n_in <- sum(!is.na(d) & d <= RADIUS_M, na.rm = TRUE) - 1L
    n_in <- max(0L, n_in)
    
    icon_file <- switch(
      s$src,
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
    
    disp <- SRC_DISPLAY[[s$src]] %||% toupper(gsub("_", " ", s$src))
    
    leaflet(options = leafletOptions(zoomControl = TRUE, preferCanvas = TRUE)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = s$lon, lat = s$lat, zoom = 15) %>%
      addMapPane("radius", zIndex = 420) %>%
      addMapPane("peers",  zIndex = 430) %>%
      addCircles(
        lng = s$lon, lat = s$lat, radius = RADIUS_M,
        color = "#3478f6", weight = 1, opacity = 0.7, fillOpacity = 0.05,
        options = pathOptions(pane = "radius")
      ) %>%
      addCircleMarkers(
        data = peer, lng = ~lon, lat = ~lat,
        radius = 6, stroke = FALSE, fillOpacity = 0.75, fillColor = "#9EC5FE",
        label = ~name,
        labelOptions = labelOptions(direction = "auto", opacity = 0.95, textsize = "13px", sticky = TRUE),
        options = pathOptions(pane = "peers")
      ) %>%
      addMarkers(
        lng = s$lon, lat = s$lat, icon = icon_selected,
        label = paste0(s$name, " (", disp, ")"),
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
    df  <- parse_opening_hours(s$openinghours %||% "")
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
    df <- parse_opening_hours(s$openinghours %||% "")
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
  
  session$onFlushed(function(){
    session$sendCustomMessage("peerCaptionText", list(text = ""))
  }, once = TRUE)
}



shinyApp(ui, server) 
