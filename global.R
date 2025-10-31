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
      
      df$open_now <- sapply(df$openinghour, parse_opening_hours )
      
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
      df$open_now <- sapply(df$openinghour, parse_opening_hours)
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





