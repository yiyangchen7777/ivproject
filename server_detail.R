# =========================================================
# detail_server.R — modular server logic for the "Detail" page
# =========================================================
detail_server <- function(id, all) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ---------------- Selected place reactive ----------------
    sel <- reactive({
      nm <- input$pick
      validate(need(!is.null(nm) && nzchar(nm), ""))
      all %>% filter(name == nm) %>% slice(1)
    })
    
    # ---------------- Image carousel ----------------
    output$shopImage <- renderUI({
      if (is.null(input$pick) || input$pick == "") {
        return(
          div(
            class = "glass",
            style = "height:70vh;display:flex;flex-direction:column;justify-content:center;align-items:center;text-align:center;",
            h2(
              style = "font-family:'Sorts Mill Goudy',serif;font-size:36px;color:#1e3a8a;margin-bottom:12px;",
              "Discover Melbourne’s Best "
            ),
            p(
              style = "font-family:'Inter',sans-serif;color:#555;font-size:16px;max-width:400px;",
              "Select a place from the map or list to explore its details."
            ),
            tags$img(
              src = "data/melbourne-skyline.jpg",
              style = "max-width:300px;margin-top:20px;opacity:0.85;border-radius:16px;"
            )
          )
        )
      }
      
      s <- sel()
      pid <- s$place_id %||% ""
      src_dir <- s$src %||% ""
      imgs <- character(0)
      
      if (nzchar(pid) && nzchar(src_dir)) {
        fs_dir <- file.path(src_dir, pid)
        if (dir.exists(fs_dir)) {
          files <- list.files(
            fs_dir,
            pattern = "(?i)\\.(jpg|jpeg|png|gif|webp)$",
            full.names = FALSE
          )
          if (length(files)) {
            ord <- order(
              suppressWarnings(as.numeric(gsub("\\D+", "", files))),
              files, na.last = TRUE
            )
            files <- unique(files[ord])
          }
          imgs <- file.path(src_dir, pid, files)
        }
      }
      
      if (length(imgs) == 0) {
        return(div(
          class = "shop-img",
          style = "display:flex;align-items:center;justify-content:center;color:#aaa;",
          "No image"
        ))
      }
      
      cid <- paste0("carousel_", as.integer(runif(1, 1, 1e9)))
      n <- length(imgs)
      
      items <- lapply(seq_along(imgs), function(i) {
        classes <- if (i == 1) "carousel-item active" else "carousel-item"
        counter <- div(
          style = "position:absolute; right:10px; top:10px; background:rgba(0,0,0,0.45);
                   color:#fff; font-size:12px; padding:2px 6px; border-radius:999px; z-index:5;",
          paste(i, "/", n)
        )
        div(
          class = classes,
          div(
            style = "position:relative;",
            counter,
            img(
              src = imgs[[i]],
              class = "d-block w-100 shop-img",
              onerror = "this.onerror=null; this.src='data:image/svg+xml;utf8,
                         <svg xmlns=%22http://www.w3.org/2000/svg%22 width=%22640%22 height=%22320%22>
                         <rect width=%22100%25%22 height=%22100%25%22 fill=%22%23f2f2f2%22/>
                         <text x=%2250%25%22 y=%2250%25%22 font-size=%2220%22 text-anchor=%22middle%22
                         fill=%22%23999%22 dy=%22.35em%22>No image</text></svg>';"
            )
          )
        )
      })
      
      indicators <- if (n > 1) {
        lapply(seq_len(n), function(i) {
          tags$button(
            type = "button",
            `data-bs-target` = paste0("#", cid),
            `data-bs-slide-to` = i - 1,
            class = if (i == 1) "active" else NULL,
            `aria-current` = if (i == 1) "true" else NULL,
            `aria-label` = paste("Slide", i)
          )
        })
      }
      
      div(
        id = cid, class = "carousel slide",
        `data-bs-ride` = if (n > 1) "carousel" else NULL,
        `data-bs-interval` = if (n > 1) "4000" else NULL,
        `data-bs-touch` = "true",
        if (!is.null(indicators)) div(class = "carousel-indicators", indicators),
        div(class = "carousel-inner", items),
        if (n > 1) tagList(
          tags$button(
            class = "carousel-control-prev", type = "button",
            `data-bs-target` = paste0("#", cid), `data-bs-slide` = "prev",
            tags$span(class = "carousel-control-prev-icon", `aria-hidden` = "true"),
            tags$span(class = "visually-hidden", "Previous")
          ),
          tags$button(
            class = "carousel-control-next", type = "button",
            `data-bs-target` = paste0("#", cid), `data-bs-slide` = "next",
            tags$span(class = "carousel-control-next-icon", `aria-hidden` = "true"),
            tags$span(class = "visually-hidden", "Next")
          )
        )
      )
    })
    
    # ---------------- Website / phone / address ----------------
    output$linkPhoneAddress <- renderText({
      s <- sel()
      link  <- if (nzchar(s$website %||% "")) {
        sprintf("<a href='%s' target='_blank'>Website ↗</a>", s$website)
      } else "<span class='muted'>No Website Info</span>"
      
      phone <- if (nzchar(s$phone %||% "")) s$phone else "<span class='muted'>No Phone Info</span>"
      addr  <- if (nzchar(s$address %||% "")) s$address else "<span class='muted'>No Address Info</span>"
      
      sprintf("<div><div>%s</div><div class='muted'>%s</div><div class='muted'>%s</div></div>",
              link, phone, addr)
    })
    
    # ---------------- Description ----------------
    output$shopDesc <- renderUI({
      s <- sel()
      txt <- s$description %||% "No Description Info"
      HTML(htmlEscape(txt))
    })
    
    # ---------------- KPIs (price range & category) ----------------
    output$kpisText <- renderUI({
      s <- sel()
      pr_txt <- price_range(s$price_level)
      catg <- s$category %||% "N/A"
      HTML(paste0(
        "<div style='height:10px'></div>",
        "<div><span class='kpi'>Price range: ", htmlEscape(pr_txt), "</span></div>",
        "<div style='height:10px'></div>",
        "<div><span class='kpi'>Category: ", htmlEscape(catg), "</span></div>"
      ))
    })
    
    # ---------------- Rating progress bar ----------------
    output$ratingProgress <- renderUI({
      s <- sel()
      sc <- suppressWarnings(as.numeric(s$rating))
      sc <- ifelse(is.na(sc), 0, sc)
      pct <- max(0, min(100, round(100 * sc / 5, 1)))
      uid <- paste0("rb_", as.integer(runif(1, 1, 1e9)))
      
      HTML(sprintf('
        <div class="rating-wrap">
          <div class="rating-label">Rating: <span>%s</span> / 5</div>
          <div id="%s_wrap" style="position:relative; width:25%%;">
            <div id="%s_outer" class="progress-outer" style="overflow:hidden;">
              <div id="%s_inner" class="progress-inner"></div>
            </div>
            <div id="%s_rating_marker"
                 style="position:absolute; top:22px; font-size:12px; color:#444;
                        left:0; transform:translateX(-50%%);">%s</div>
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
    
    # ---------------- Peer map ----------------
    hav_dist_m <- function(lat1, lon1, lat2, lon2) {
      rad <- pi / 180
      dlat <- (lat2 - lat1) * rad
      dlon <- (lon2 - lon1) * rad
      a <- sin(dlat / 2)^2 + cos(lat1 * rad) * cos(lat2 * rad) * sin(dlon / 2)^2
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
          iconUrl = icon_file,
          iconWidth = 48, iconAnchorX = 24, iconAnchorY = 48
        )
      } else {
        icon_selected <- makeAwesomeIcon(icon = "map-marker-alt",
                                         markerColor = "blue", iconColor = "white")
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
          labelOptions = labelOptions(
            direction = "auto", opacity = 0.95, textsize = "13px", sticky = TRUE
          ),
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
    
    # ---------------- Open/closed status ----------------
    output$openNowRight <- renderUI({
      s <- sel()
      df <- parse_opening_hours(s$openinghours %||% "")
      now <- with_tz(Sys.time(), "Australia/Melbourne")
      open_flag <- is_open_at(df, now)
      info <- today_hours_and_next_open(df, now)
      
      badge <- if (open_flag)
        tags$span(
          style = "display:inline-block;background:#28a745;color:#fff;
                   border-radius:999px;padding:2px 10px;font-weight:600;margin-right:10px;",
          "Open now"
        )
      else
        tags$span(
          style = "display:inline-block;background:#dc3545;color:#fff;
                   border-radius:999px;padding:2px 10px;font-weight:600;margin-right:10px;",
          "Closed"
        )
      
      div(
        badge,
        if (!open_flag && nzchar(info$next_open))
          tags$div(class = "muted", paste("Next open:", info$next_open))
      )
    })
    
    # ---------------- Weekly opening hours plot ----------------
    output$openHoursPlot <- renderPlot({
      s <- sel()
      df <- parse_opening_hours(s$openinghours %||% "")
      req(nrow(df) > 0)
      
      seg <- expand_overnight_for_plot(df)
      lvl <- c("MON", "TUE", "WED", "THU", "FRI", "SAT", "SUN")
      seg$day <- toupper(seg$day)
      seg <- seg[seg$day %in% lvl, ]
      seg$day <- factor(seg$day, levels = lvl, ordered = TRUE)
      
      now_hr <- hour(with_tz(Sys.time(), "Australia/Melbourne")) +
        minute(with_tz(Sys.time(), "Australia/Melbourne")) / 60
      
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
        scale_x_continuous(
          limits = c(0, 24),
          breaks = seq(0, 24, 4),
          labels = paste0(seq(0, 24, 4), ":00")
        ) +
        scale_y_discrete(
          limits = rev(lvl),
          expand = expansion(add = c(0.05, 0.35))
        ) +
        coord_cartesian(clip = "off") +
        theme_minimal(base_size = 11) +
        labs(x = "Time of Day", y = NULL, title = "Weekly Opening Hours") +
        theme(
          plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "#F3F3F3", colour = NA),
          plot.background = element_rect(fill = "#F3F3F3", colour = NA),
          plot.margin = margin(t = 16, r = 8, b = 8, l = 8)
        )
    })
    
    # ---------------- Clear peer caption on initialization ----------------
    session$onFlushed(function() {
      session$sendCustomMessage("peerCaptionText", list(text = ""))
    }, once = TRUE)
  })
}
