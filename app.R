library(shiny)
library(leaflet)
library(dplyr)
library(htmltools)
library(shinyjs)
library(stringr)
library(lubridate)
library(jsonlite)
library(geosphere)
library(htmlwidgets)

# ---------- 数据 ----------
read_places <- function(path, type_label) {
  df <- read.csv(path, stringsAsFactors = FALSE)
  df$type <- type_label
  df
}
df_cafe       <- read_places("cafe_brunch_bakery_desc.csv", "Cafe")
df_bars       <- read_places("melbourne_cbd_bars.csv", "Bar")
df_milktea    <- read_places("milktea_juice_english_clean.csv", "Milktea")
df_restaurant <- read_places("restaurant_english_clean_desc.csv", "Restaurant")

# places <- bind_rows(df_cafe, df_bars, df_milktea, df_restaurant) %>%
#   filter(!is.na(lat), !is.na(lon))

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

print(names(places))

# ---------- 判断营业状态 ----------
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

# ---------- UI ----------
ui <- fluidPage(
  useShinyjs(),
  titlePanel("🍽️ Melbourne CBD Food & Drink Map"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      tags$div(
        id = "filter-panel",
        tags$label("Select Categories: 🍴"),
        tags$div(class = "main-block",
                 checkboxInput("main_all", "All", value = TRUE)
        ),
        tags$div(class = "main-block",
                 checkboxInput("main_restaurant", "Restaurant", value = FALSE),
                 tags$div(id = "sub_restaurant", class = "sub-option")
        ),
        tags$div(class = "main-block",
                 checkboxInput("main_bar", "Bar", value = FALSE),
                 tags$div(id = "sub_bar", class = "sub-option")
        ),
        tags$div(class = "main-block",
                 checkboxInput("main_cafe", "Cafe", value = FALSE),
                 tags$div(id = "sub_cafe", class = "sub-option")
        ),
        tags$div(class = "main-block",
                 checkboxInput("main_milktea", "Milktea", value = FALSE),
                 tags$div(id = "sub_milktea", class = "sub-option")
        ),
        tags$div(id = "choose_hint", style = "display:none; color:#777; margin-top:6px;")
      ),
      br(),
      
      div(id = "rating-section",
          tags$label("Select Rating: ⭐"),  
          selectInput(
            "rating_filter", NULL,
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
          ),
      ),
      
      # ---- Opening status filter ----
      div(
        id = "open-filter",
        tags$label("Opening Status: 🟢"),
        checkboxInput(
          "show_open_now", 
          "Show Open", 
          value = FALSE
        )
      ),
      
      # ---- select places range filter ----
      div( 
        id = "regional-range",
        selectInput(
          "radius_select", "Select Regional Range:",
          choices = c("500m" = 500, "1km" = 1000, "2km" = 2000), 
          selected = 500, width = "160px"
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
        div(
          id = "loading-overlay",
          div(class = "spinner-border text-primary", role = "status"),
          h4("Loading Map...", style = "color:#3478f6; margin-top:10px;")
        ),
        leafletOutput("map", height = 650)
      )
    )
  ),
  
  # ---- CSS + JS ----
  tags$head(HTML('
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
        margin-top: 4px !important;      /* 从默认 ~10px 减少为 4px */
        margin-bottom: 1px !important;   /* 减少底部空白 */
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
        max-width: 250px !important;  /* 左栏更窄 */
        padding-right: 8px !important; /* 减小与右侧间距 */
        margin-right: 0 !important;
      }
      
      #sidebarLayout { 
        max-height: 350px !important;
      }
      
      /* “Select Ratings” 白框样式（与 Select Categories 一致） */
      #rating-section {
        background-color: #f8f9fa;
        border-radius: 10px;
        padding: 10px 12px;              /* 整体内部间距缩小 */
        box-shadow: 0 2px 6px rgba(0,0,0,0.1);
        margin-bottom: 10px;
        font-weight: 600;
        color: #333;
      }
      
      /* 让下拉框和标题之间间距变小 */
      #rating-section .shiny-input-container {
        margin-top: 4px !important;      /* 从默认 ~10px 减少为 4px */
        margin-bottom: 4px !important;   /* 减少底部空白 */
      }
      
      /* 下拉框本身也稍微压紧 */
      #rating-section select {
        padding: 4px 8px;     /* 控制选择框内部的高度 */
        font-size: 14px;
        padding-bottom: 4px;
      }
      
      /* “Opening Status” 白框样式（保持整体风格但更紧凑） */
      #open-filter { 
        background-color: #f8f9fa;
        border-radius: 10px;
        padding: 10px 12px;              /* 整体内部间距缩小 */
        box-shadow: 0 2px 6px rgba(0,0,0,0.1);
        margin-bottom: 10px;
        font-weight: 600;
        color: #333;
        padding-bottom: 4px;
      }
      
      /* 让复选框与标题之间的间距更小 */
      #open-filter .shiny-input-container {
        margin-top: 4px !important;      /* 从默认10-12px 改成 4px */
        margin-bottom: 4px !important;   /* 缩小底部空白 */
      }
      
      /* 复选框本身字体大小、行距调整 */
      #open-filter label {
        font-size: 14px;
        font-weight: 600;
        margin-bottom: 2px;
      }
      
      /* “Select Regional Range” 白框样式（与其他卡片统一 + 底部间距更大） */
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
        height: calc(100vh - 110px) !important; /* 自动适配标题高度 */
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
        font-size: 14px;          /* 字体略小，更协调 */
        font-weight: 500;         /* 中等粗细 */
        padding: 8px 0;           /* 垂直间距更紧凑 */
        border-radius: 6px;       /* 圆角柔和 */
        border: 1px solid #ccc;   /* 增加边框，避免太空 */
        background-color: #f9f9f9;/* 浅灰背景，与白色区分 */
        transition: all 0.2s ease-in-out;
      }
      
      #clear_btn {
        font-size: 14px;          /* 字体略小，更协调 */
        font-weight: 500;         /* 中等粗细 */
        padding: 8px 0;           /* 垂直间距更紧凑 */
        border-radius: 6px;       /* 圆角柔和 */
        border: 1px solid #ccc;   /* 增加边框，避免太空 */
        background-color: #f9f9f9;/* 浅灰背景，与白色区分 */
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
      
      // ✅ 当 Shiny 完成 leaflet 渲染后隐藏加载层
      $(document).on("shiny:value", function(event){
        if(event.name === "map"){
          setTimeout(function(){
            $("#loading-overlay").fadeOut(600);
          }, 500); // 给地图一些初始化时间
        }
      });
  
    </script>
  '))
)

# ---------- SERVER ----------
server <- function(input, output, session) {
  user_loc <- reactiveVal(NULL)
  
  # 动态生成各主类的子类复选框（从数据里抓 category）
  observe({
    insert_subs <- function(type, container_id) {
      subs <- sort(unique(places$category[places$type == type]))
      if (length(subs) == 0) return()
      # 生成 label + checkbox，name=sub_<typeLower>
      type_lower <- tolower(type)
      # 对每个子类生成一条 <label><input ...> 文本；确保 HTML 转义
      labels <- vapply(subs, function(s) {
        sprintf("<label><input type='checkbox' name='sub_%s' value='%s'/> %s</label>",
                type_lower, htmlEscape(s), htmlEscape(s))
      }, character(1))
      html <- paste0("<div class='sub-option-inner'>", paste0(labels, collapse=""), "</div>")
      js <- sprintf("$('#%s').html(%s);", container_id, jsonlite::toJSON(html, auto_unbox=TRUE))
      shinyjs::runjs(js)
    }
    insert_subs("Restaurant", "sub_restaurant")
    insert_subs("Bar", "sub_bar")
    insert_subs("Cafe", "sub_cafe")
    insert_subs("Milktea", "sub_milktea")
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
    # ---- 现代底图 ----
    addProviderTiles(providers$Stadia.AlidadeSmooth, group = "Light (Modern)") %>%
      addProviderTiles(providers$Stadia.AlidadeSmoothDark, group = "Dark (Modern)") %>%
      addProviderTiles(providers$CartoDB.Voyager, group = "Voyager") %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Minimal Gray") %>%
      
    # ---- 初始视图 ----
    setView(lng = 144.9631, lat = -37.8100, zoom = 15) %>%
      
    # ---- 底图切换 ----
    addLayersControl(
      baseGroups = c("Light (Modern)", "Dark (Modern)", "Voyager", "Minimal Gray"),
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
  
      
    # ---- 🧭 回到初始点按钮 ----
    addEasyButton(
      easyButton(
        icon = "fa-bullseye",
        title = "Back to Melbourne CBD",
        onClick = JS("function(btn, map){ map.setView([-37.8100, 144.9631], 15); }")
      )
    )
    
  })
  
  
  
  
  # 当前筛选（主类+子类），再叠加半径（若已定位）
  get_filtered_df <- reactive({
    # 主类
    selected_main <- c()
    if (isTRUE(input$main_all) ||
        (!isTRUE(input$main_restaurant) && !isTRUE(input$main_bar) &&
         !isTRUE(input$main_cafe) && !isTRUE(input$main_milktea))) {
      selected_main <- c("Restaurant","Bar","Cafe","Milktea")
    } else {
      if (isTRUE(input$main_restaurant)) selected_main <- c(selected_main,"Restaurant")
      if (isTRUE(input$main_bar))        selected_main <- c(selected_main,"Bar")
      if (isTRUE(input$main_cafe))       selected_main <- c(selected_main,"Cafe")
      if (isTRUE(input$main_milktea))    selected_main <- c(selected_main,"Milktea")
    }
    df <- places %>% filter(type %in% selected_main)
    
    # 子类（仅对勾选的主类生效）
    for (t in c("Restaurant","Bar","Cafe","Milktea")) {
      sub_vals <- input[[paste0("sub_",tolower(t))]]
      if (!is.null(sub_vals) && length(sub_vals) > 0) {
        df <- df %>% filter(!(type==t) | (category %in% sub_vals))
      }
    }
    
    # 半径叠加（若已定位）
    loc <- user_loc()
    r <- as.numeric(input$radius_select)
    if (!is.null(loc) && !is.na(r) && r > 0) {
      df$dist <- geosphere::distHaversine(cbind(df$lon, df$lat), c(loc$lon, loc$lat))
      df <- df[df$dist <= r, , drop = FALSE]
    }
    
    df$open_now <- sapply(df$openinghour, is_open_today_now)
    
    # filter by ratings
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
    
    # ---- Show only open shops filter ----
    if (isTRUE(input$show_open_now)) {
      df <- df[df$open_now == TRUE, , drop = FALSE]
    }
    
    df
  })
  
  # 渲染商铺（受主类、子类、半径变化驱动）
  observe({
    df <- get_filtered_df()
    if (nrow(df) == 0) {
      leafletProxy("map", session = session) %>% clearGroup("poi_markers")
      return()
    }
    df$open_now <- sapply(df$openinghour, is_open_today_now)
    df$icon_file <- ifelse(df$open_now, paste0(df$type,"_icon.png"), paste0(df$type,"_icon_gray.png"))
    
    icons_set <- icons(iconUrl = df$icon_file, iconWidth = 40, iconHeight = 55,
                       iconAnchorX = 20, iconAnchorY = 55, popupAnchorX = 1, popupAnchorY = -55)
    popup_html <- function(row) {
      
      # remove "Australia"
      clean_address <- gsub(",\\s*Australia\\s*$", "", row$address, ignore.case = TRUE)
      
      # 评分
      rating_html <- if (!is.na(row$rating) && row$rating != "") {
        sprintf("⭐ <b>%.1f</b>", as.numeric(row$rating))
      } else {
        "<i>No Rating Info</i>"
      }
      
      # 电话
      phone_html <- if (!is.na(row$phone) && row$phone != "") {
        sprintf("📞 %s", htmlEscape(row$phone))
      } else {
        "<i>No Phone Info</i>"
      }
      
      # 网站
      website_html <- if (!is.na(row$website) && row$website != "") {
        sprintf('<a href="%s" target="_blank">🔗 Visit Website</a>', htmlEscape(row$website))
      } else {
        "<i>No Website Info</i>"
      }
      
      link <- sprintf('<a href="https://www.google.com/maps/dir/?api=1&destination=%f,%f" target="_blank">🚗 Map</a>',
                      row$lat, row$lon)
      # 整合弹窗内容
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
        lng = df$lon, lat = df$lat, icon = icons_set,
        popup = lapply(seq_len(nrow(df)), function(i) popup_html(df[i,])),
        options = markerOptions(className = "poi-marker"),
        clusterOptions = markerClusterOptions(),
        group = "poi_markers"
      )
  })
  
  # 📍 定位叠加层（不破坏商铺层），并自动飞过去
  observeEvent(input$user_location, {
    loc <- input$user_location
    if (is.null(loc$lat) || is.null(loc$lon)) return()
    user_loc(loc)
    r <- as.numeric(input$radius_select)
    
    leafletProxy("map", session = session) %>%
      clearGroup("user_marker") %>%
      clearGroup("range_circle") %>%
      addMarkers(
        lng = loc$lon, lat = loc$lat,
        icon = icons(
          iconUrl = "https://unpkg.com/leaflet@1.9.3/dist/images/marker-icon.png",
          iconWidth = 25, iconHeight = 41,
          iconAnchorX = 12, iconAnchorY = 20
        ),
        label = "You are here 📍",
        options = markerOptions(className = "user-location", clickable = FALSE),
        group = "user_marker"
      ) %>%
      addCircles(
        lng = loc$lon, lat = loc$lat,
        radius = r, color = "#3478f6", fillColor = "#9EC5FE", fillOpacity = 0.3,
        group = "range_circle"
      ) %>%
      flyTo(lng = loc$lon, lat = loc$lat, zoom = 15)
    # 👉 不在这里重绘 poi_markers，因为上面 observe() 已订阅 user_loc()/radius/input 变化，会自动重绘为“圆内+筛选”的集合
  })
  
  # 半径变化：只更新圆（poi 渲染由上面 observe() 统一负责）
  observeEvent(input$radius_select, {
    loc <- user_loc()
    if (is.null(loc)) return()
    r <- as.numeric(input$radius_select)
    leafletProxy("map", session = session) %>%
      clearGroup("range_circle") %>%
      addCircles(
        lng = loc$lon, lat = loc$lat,
        radius = r, color = "blue", fillColor = "skyblue", fillOpacity = 0.3,
        group = "range_circle"
      )
  })
  
  # ❌ Clear Location：仅清除定位层 + 回到 CBD（商铺保持当前筛选/子类状态）
  observeEvent(input$clear_btn, {
    user_loc(NULL)
    leafletProxy("map", session = session) %>%
      clearGroup("user_marker") %>%
      clearGroup("range_circle") %>%
      setView(lng = 144.9631, lat = -37.8100, zoom = 15)
    # poi_markers 不动；上面的 observe() 会因 user_loc 变为 NULL 自动取消半径限制并维持当前主/子类过滤
  })
}

shinyApp(ui, server, options = list(launch.browser = TRUE))
