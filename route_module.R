load_route_data <- function() {
  cafes <- read.csv("cafe_brunch_bakery_desc.csv", stringsAsFactors = FALSE)
  bars <- read.csv("melbourne_cbd_bars.csv", stringsAsFactors = FALSE)
  drinks <- read.csv("milktea_juice_english_clean.csv", stringsAsFactors = FALSE)
  restaurants <- read.csv("restaurant_english_clean_desc.csv", stringsAsFactors = FALSE)

  cafes$category <- "Cafe/Brunch"
  bars$category <- "Bar"

  bind_rows(cafes, bars, drinks, restaurants) %>%
    filter(!is.na(lat) & !is.na(lon) & !is.na(name)) %>%
    mutate(
      rating = as.numeric(rating),
      rating = ifelse(is.na(rating), 3.5, rating),
      price_level = dplyr::case_when(
        is.na(price_level) | price_level == "" ~ "N/A",
        grepl("INEXPENSIVE", price_level, ignore.case = TRUE) ~ "$",
        grepl("MODERATE", price_level, ignore.case = TRUE) ~ "$$",
        grepl("EXPENSIVE", price_level, ignore.case = TRUE) ~ "$$$",
        grepl("VERY_EXPENSIVE", price_level, ignore.case = TRUE) ~ "$$$$",
        TRUE ~ "$$"
      ),
      price_numeric = dplyr::case_when(
        price_level == "$" ~ 1,
        price_level == "$$" ~ 2,
        price_level == "$$$" ~ 3,
        price_level == "$$$$" ~ 4,
        price_level == "N/A" ~ 2,
        TRUE ~ 2
      )
    )
}

route_default_meal_time <- function() {
  current_hour <- as.numeric(format(Sys.time(), "%H"))
  if (current_hour >= 7 && current_hour < 10) {
    "Breakfast (7-10 AM)"
  } else if (current_hour >= 10 && current_hour < 14) {
    "Lunch (12-2 PM)"
  } else if (current_hour >= 14 && current_hour < 21) {
    "Dinner (6-9 PM)"
  } else if (current_hour >= 21 || current_hour < 7) {
    "Late Night (9 PM+)"
  } else {
    "Anytime"
  }
}

route_recommend_venues <- function(data, budget, use_location, meal_time, user_coords = NULL, search_text = "") {
  budget_filter <- dplyr::case_when(
    budget == "Low" ~ 1,
    budget == "Medium" ~ 2,
    budget == "High" ~ 3,
    budget == "Luxury" ~ 4,
    TRUE ~ 2
  )

  time_categories <- switch(
    meal_time,
    "Breakfast (7-10 AM)" = c("Cafe/Brunch", "Cafe"),
    "Lunch (12-2 PM)"    = c("Cafe/Brunch", "Cafe", "Other", "Mexican"),
    "Dinner (6-9 PM)"    = c("Other", "Bar", "Mexican"),
    "Late Night (9 PM+)" = c("Bar"),
    c("Cafe/Brunch", "Cafe", "Bar", "Other", "Mexican", "Milk Tea", "Juice")
  )

  filtered <- data %>%
    filter(price_numeric <= budget_filter + 1) %>%
    filter(category %in% time_categories | grepl(paste(time_categories, collapse = "|"), category, ignore.case = TRUE))

  if (!is.null(search_text) && nzchar(search_text)) {
    filtered <- filtered %>%
      filter(grepl(search_text, name, ignore.case = TRUE) |
               grepl(search_text, category, ignore.case = TRUE) |
               grepl(search_text, address, ignore.case = TRUE))
  }

  if (isTRUE(use_location) && !is.null(user_coords)) {
    center_lat <- user_coords$lat
    center_lon <- user_coords$lng

    filtered <- filtered %>%
      mutate(
        distance = (distHaversine(cbind(lon, lat), c(center_lon, center_lat)) / 1000) * 1.3
      )
  } else {
    filtered$distance <- 0
  }

  filtered %>%
    mutate(
      rating_score = (rating / 5) * 0.4,
      price_score = (1 - abs(price_numeric - budget_filter) / 4) * 0.3,
      distance_score = ifelse(distance > 0, (1 - pmin(distance / 5, 1)) * 0.2, 0.2),
      random_score = runif(n()) * 0.1,
      total_score = rating_score + price_score + distance_score + random_score
    ) %>%
    arrange(desc(total_score))
}

route_get_route_from_osrm <- function(from_lon, from_lat, to_lon, to_lat) {
  tryCatch({
    url <- sprintf(
      "http://router.project-osrm.org/route/v1/driving/%f,%f;%f,%f?overview=full&geometries=geojson",
      from_lon, from_lat, to_lon, to_lat
    )
    response <- jsonlite::fromJSON(url)
    if (response$code == "Ok" && length(response$routes) > 0) {
      route <- response$routes[1, ]
      return(list(
        distance = route$distance / 1000,
        duration = route$duration / 60,
        geometry = route$geometry$coordinates[[1]]
      ))
    }
    NULL
  }, error = function(e) NULL)
}

route_calculate_route <- function(selected_venues) {
  if (nrow(selected_venues) < 2) {
    return(list(
      total_distance = 0,
      total_walk_time = 0,
      total_drive_time = 0,
      route_details = data.frame(),
      route_geometries = list()
    ))
  }

  route_details <- data.frame()
  route_geometries <- list()
  total_distance <- 0

  for (i in 1:(nrow(selected_venues) - 1)) {
    from <- selected_venues[i, ]
    to <- selected_venues[i + 1, ]

    osrm_route <- route_get_route_from_osrm(from$lon, from$lat, to$lon, to$lat)

    if (!is.null(osrm_route)) {
      distance <- osrm_route$distance
      drive_time <- osrm_route$duration
      walk_time <- (distance / 5) * 60
      route_geometries[[i]] <- osrm_route$geometry
    } else {
      straight_distance <- distHaversine(c(from$lon, from$lat), c(to$lon, to$lat)) / 1000
      distance <- straight_distance * 1.3
      walk_time <- (distance / 5) * 60
      drive_time <- (distance / 30) * 60
      route_geometries[[i]] <- NULL
    }

    route_details <- rbind(
      route_details,
      data.frame(
        from = from$name,
        to = to$name,
        distance_km = round(distance, 2),
        walk_time_min = round(walk_time, 1),
        drive_time_min = round(drive_time, 1)
      )
    )

    total_distance <- total_distance + distance
  }

  list(
    total_distance = round(total_distance, 2),
    total_walk_time = round(sum(route_details$walk_time_min), 1),
    total_drive_time = round(sum(route_details$drive_time_min), 1),
    route_geometries = route_geometries,
    route_details = route_details
  )
}

route_module_ui <- function(id) {
  ns <- NS(id)
  message_id <- sprintf("route_get_location_%s", id)
  tagList(
    tags$head(
      tags$script(src = "https://cdn.jsdelivr.net/npm/sortablejs@1.15.0/Sortable.min.js"),
      tags$script(HTML(sprintf("
        Shiny.addCustomMessageHandler('%s', function(message){
          if (navigator.geolocation) {
            navigator.geolocation.getCurrentPosition(
              function(position){
                Shiny.setInputValue('%s', {
                  lat: position.coords.latitude,
                  lng: position.coords.longitude,
                  ts: Date.now()
                }, {priority:'event'});
              },
              function(error){
                alert('Unable to get location: ' + error.message);
              },
              {enableHighAccuracy: true, maximumAge: 0, timeout: 10000}
            );
          } else {
            alert('Geolocation is not supported by this browser.');
          }
        });

        $(document).on('shiny:value', function(event){
          if (event.name === '%s') {
            setTimeout(function(){
              var el = document.getElementById('%s');
              if (el && !el.sortableInstance) {
                el.sortableInstance = Sortable.create(el, {
                  animation: 150,
                  ghostClass: 'sortable-ghost',
                  dragClass: 'sortable-drag',
                  handle: '.sortable-item',
                  onEnd: function(evt){
                    var items = el.querySelectorAll('.sortable-item');
                    var order = [];
                    items.forEach(function(item){
                      order.push(item.getAttribute('data-venue-name'));
                    });
                    Shiny.setInputValue('%s', order, {priority:'event'});
                  }
                });
              }
            }, 100);
          }
        });
      ", message_id, ns("user_coords"), ns("selected_venues_ui"), ns("sortable-venues-container"), ns("venue_order")))),
      tags$style(HTML("
        body,
        .route-sidebar,
        .route-box,
        .info-box,
        .leaflet-container,
        .route-summary-card {
          font-family: 'Poppins', sans-serif;
        }
        .route-sidebar .form-control {
          background-color:#ffffff;
          color:#2c3e50;
          border:1px solid #d0d5dd;
          border-radius:10px;
          padding:8px 12px;
          box-shadow:none;
        }
        .route-sidebar .form-control:focus {
          border-color:#3478f6;
          box-shadow:0 0 0 3px rgba(52,120,246,0.15);
        }
        .route-sidebar .selectize-input {
          width:100% !important;
          border:1px solid #d0d5dd !important;
          border-radius:10px !important;
          padding:8px 12px !important;
          box-shadow:none !important;
          background-color:#ffffff !important;
        }
        .route-sidebar .selectize-input.focus {
          border-color:#3478f6 !important;
          box-shadow:0 0 0 3px rgba(52,120,246,0.15) !important;
        }
        .route-sidebar .selectize-dropdown {
          border-radius:10px !important;
          border:1px solid #d0d5dd !important;
        }
        .route-sidebar .form-group {
          margin-bottom:16px;
        }
        .route-sidebar .shiny-input-container {
          width:100% !important;
        }
        .route-sidebar .selectize-control {
          width:100% !important;
        }
        .route-optimize-btn {
          padding:10px 24px;
          font-size:14px;
          letter-spacing:0.5px;
          border-radius:10px;
          background:#3478f6;
          border:none;
          color:white;
          box-shadow:0 4px 10px rgba(52,120,246,0.25);
          transition:all .3s ease;
          white-space:nowrap;
        }
        .route-optimize-btn:hover {
          background:#265ed2;
          box-shadow:0 6px 14px rgba(52,120,246,0.35);
          transform:translateY(-1px);
        }
        .route-optimize-btn:active {
          transform:translateY(0);
          box-shadow:0 3px 6px rgba(52,120,246,0.3);
        }
        .route-sidebar label { color:#1f2933; font-weight:500; font-size:13px; }
        .route-box { border-radius:0; box-shadow:none; border:1px solid #e0e0e0; background:#ffffff; }
        .route-box .box-header { border-bottom:1px solid #e0e0e0; background:#ffffff; }
        .route-box .box-title { font-weight:300; font-size:16px; color:#2c3e50; letter-spacing:0.5px; }
        .route-sortable .sortable-item { transition:all 0.2s ease; }
        .route-sortable .sortable-item:hover { box-shadow:0 2px 8px rgba(0,0,0,0.1); }
        .route-sortable .sortable-ghost { opacity:0.4; background-color:#ecf0f1; }
        .route-sortable .sortable-drag { opacity:0.8; box-shadow:0 4px 12px rgba(0,0,0,0.15); }
        .route-summary-card { display:flex; justify-content:space-between; align-items:center; padding:10px 12px; background:#f8f9fa; margin-bottom:10px; border:1px solid #e0e0e0; }
        .route-summary-card h4 { margin:0; font-size:14px; font-weight:400; color:#2c3e50; }
        .route-summary-card span { font-size:12px; color:#7f8c8d; }
        .route-clear-btn { border-radius:6px; background:#fff; border:1px solid #95a5a6; color:#2c3e50; font-weight:400; letter-spacing:0.5px; }
        .route-location-card {
          background-color:#f8f9fa;
          border-radius:10px;
          padding:12px;
          box-shadow:0 2px 6px rgba(0,0,0,0.08);
          margin-bottom:16px;
        }
        .route-location-card label {
          font-weight:500;
          font-size:13px;
          color:#1f2933;
          margin-bottom:8px;
          display:block;
        }
        .route-location-actions {
          display:flex;
          gap:8px;
        }
        .route-location-actions .btn {
          flex:1;
          font-size:13px;
          letter-spacing:0.3px;
        }
        .route-locate-btn {
          background:#3478f6;
          color:#ffffff;
          border:1px solid #2d6ddf;
          border-radius:8px;
          box-shadow:0 2px 6px rgba(52,120,246,0.25);
        }
        .route-locate-btn:hover {
          background:#2d6ddf;
          color:#ffffff;
        }
        .route-location-clear {
          background:#ffffff;
          color:#3478f6;
          border:1px solid #3478f6;
          border-radius:8px;
        }
        .route-location-clear:hover {
          background:#3478f6;
          color:#ffffff;
        }
        .route-location-status {
          display:block;
          font-size:12px;
          margin-top:10px;
          word-break:break-word;
        }
        .route-location-status--inactive { color:#95a5a6; font-style:italic; }
        .route-location-status--active { color:#2c3e50; }
        .route-legend {
          background:rgba(255,255,255,0.92);
          padding:6px 8px;
          border-radius:10px;
          box-shadow:0 4px 12px rgba(15,23,42,0.12);
          font-size:11px;
          color:#1f2933;
          min-width:110px;
        }
        .route-legend-item {
          display:flex;
          align-items:center;
          gap:6px;
          margin-bottom:3px;
        }
        .route-legend-item img {
          width:22px;
          height:26px;
          object-fit:contain;
        }
        .route-legend-item:last-child {
          margin-bottom:0;
        }
      "))
    ),
    fluidRow(
      column(
        width = 3,
        div(
          class = "route-sidebar",
          textInput(ns("search_text"), "Search:", placeholder = "Enter restaurant name...", width = "100%"),
          selectInput(
            ns("budget"), "Budget:",
            choices = c("Low ($)", "Medium ($$)", "High ($$$)", "Luxury ($$$$)"),
            selected = "Medium ($$)",
            width = "100%"
          ),
          div(
            class = "route-location-card",
            tags$label("Location:"),
            div(
              class = "route-location-actions",
              actionButton(ns("locate_btn"), "Locate Me", class = "route-locate-btn"),
              actionButton(ns("clear_location"), "Clear", class = "route-location-clear")
            ),
            uiOutput(ns("location_status"))
          ),
          selectInput(
            ns("meal_time"), "Meal Time:",
            choices = c(
              "Breakfast (7-10 AM)",
              "Lunch (12-2 PM)",
              "Dinner (6-9 PM)",
              "Late Night (9 PM+)",
              "Anytime"
            ),
            selected = route_default_meal_time(),
            width = "100%"
          ),
          actionButton(ns("clear_selection"), "CLEAR SELECTION", class = "route-clear-btn", width = "100%"),
          tags$hr(),
          div(
            style = "font-size:11px; color:#34495e;",
            icon("info-circle"),
            " Recommendations update automatically when you change filters."
          )
        )
      ),
      column(
        width = 9,
        fluidRow(
          shinydashboard::box(
            title = "SELECTED VENUES",
            width = 5,
            solidHeader = FALSE,
            status = "primary",
            class = "route-box",
            height = 600,
            uiOutput(ns("trip_summary")),
            div(
              style = "display:flex; justify-content:flex-end; padding:8px 10px; margin-bottom:10px;",
              actionButton(
                ns("optimize_route"),
                "OPTIMIZE ROUTE",
                icon = icon("route"),
                class = "route-optimize-btn",
                style = "padding:10px 24px; font-size:14px; letter-spacing:0.5px; border-radius:10px; background:#3478f6; border:none; color:white; box-shadow:0 4px 10px rgba(52,120,246,0.25); transition:all .3s ease;"
              )
            ),
            div(style = "height:400px; overflow-y:auto;", class = "route-sortable",
                uiOutput(ns("selected_venues_ui"))
            )
          ),
          shinydashboard::box(
            title = "RECOMMENDED VENUES MAP",
            width = 7,
            solidHeader = FALSE,
            status = "primary",
            class = "route-box",
            height = 600,
            leafletOutput(ns("map"), height = 520)
          )
        ),
        fluidRow(
          shinydashboard::box(
            title = "ROUTE DETAILS",
            width = 12,
            solidHeader = FALSE,
            status = "primary",
            class = "route-box",
            DTOutput(ns("route_table"))
          )
        )
      )
    )
  )
}

route_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    message_id <- sprintf("route_get_location_%s", id)

    all_venues <- reactiveVal(load_route_data())
    recommendations <- reactiveVal(data.frame())
    selected_venues <- reactiveVal(data.frame())
    user_location <- reactiveVal(NULL)

    category_icon_name <- function(cat, selected = FALSE) {
      base <- "Restaurant"
      cat <- ifelse(is.na(cat), "", cat)
      if (grepl("Bar", cat, ignore.case = TRUE)) {
        base <- "Bar"
      } else if (grepl("Cafe|Coffee|Brunch|Bakery", cat, ignore.case = TRUE)) {
        base <- "Cafe"
      } else if (grepl("Milk Tea|Juice|Drink", cat, ignore.case = TRUE)) {
        base <- "Milktea"
      } else {
        base <- "Restaurant"
      }
      suffix <- if (isTRUE(selected)) "_icon_red.png" else "_icon.png"
      paste0(base, suffix)
    }

    observeEvent(input$locate_btn, {
      session$sendCustomMessage(message_id, list())
    })

    observeEvent(input$clear_location, {
      user_location(NULL)
      leafletProxy(ns("map"), session = session) %>%
        clearGroup("user_location") %>%
        setView(lng = 144.9631, lat = -37.8136, zoom = 14)
      showNotification("Location cleared.", type = "warning", duration = 2)
    })

    observeEvent(input$user_coords, {
      coords <- input$user_coords
      if (!is.null(coords)) {
        user_location(list(
          name = "My GPS Location",
          lat = coords$lat,
          lng = coords$lng
        ))
        showNotification("GPS location detected successfully!", type = "message", duration = 3)
      }
    })

    observe({
      loc <- user_location()
      proxy <- leafletProxy(ns("map"), session = session)
      proxy <- proxy %>% clearGroup("user_location")
      if (!is.null(loc)) {
        proxy %>%
          addAwesomeMarkers(
            lng = loc$lng,
            lat = loc$lat,
            icon = awesomeIcons(icon = "user", iconColor = "white", library = "fa", markerColor = "blue"),
            popup = "<strong>Your Location</strong>",
            label = "You are here",
            layerId = "user_location",
            group = "user_location"
          ) %>%
          setView(lng = loc$lng, lat = loc$lat, zoom = 15)
      }
    })

    output$location_status <- renderUI({
      loc <- user_location()
      if (is.null(loc)) {
        tags$span("Location not set", class = "route-location-status route-location-status--inactive")
      } else {
        label <- if (!is.null(loc$name)) loc$name else "Custom Location"
        tags$span(
          sprintf("%s (%.4f, %.4f)", label, loc$lat, loc$lng),
          class = "route-location-status route-location-status--active"
        )
      }
    })

    observe({
      req(all_venues())
      input$budget
      input$meal_time
      input$search_text
      user_location()

      recs <- route_recommend_venues(
        all_venues(),
        input$budget,
        use_location = !is.null(user_location()),
        input$meal_time,
        user_coords = user_location(),
        search_text = input$search_text
      )
      recommendations(recs)
    })

    observeEvent(input$clear_selection, {
      selected_venues(data.frame())
      showNotification("Selection cleared!", type = "warning", duration = 2)
    })

    output$trip_summary <- renderUI({
      selected <- selected_venues()
      if (nrow(selected) == 0) return(NULL)

      cafes <- sum(grepl("Cafe|Coffee|Brunch|Bakery", selected$category, ignore.case = TRUE))
      bars <- sum(grepl("Bar", selected$category, ignore.case = TRUE))
      drinks <- sum(grepl("Milk Tea|Juice|Drink", selected$category, ignore.case = TRUE))
      restaurants <- nrow(selected) - cafes - bars - drinks

      div(
        class = "route-summary-card",
        div(
          h4("Trip Overview"),
          tags$span(sprintf("%d venues selected", nrow(selected)))
        ),
        div(
          tags$span(sprintf("🍽️ %d Restaurant%s", restaurants, if (restaurants != 1) "s" else "")),
          tags$span(sprintf("☕ %d Cafe%s", cafes, if (cafes != 1) "s" else "")),
          tags$span(sprintf("🍸 %d Bar%s", bars, if (bars != 1) "s" else "")),
          tags$span(sprintf("🧋 %d Drink%s", drinks, if (drinks != 1) "s" else ""))
        )
      )
    })

    output$selected_venues_ui <- renderUI({
      selected <- selected_venues()
      if (nrow(selected) == 0) {
        return(
          div(
            style = "text-align:center; padding:80px 20px;",
            h4("No venues selected", style = "color:#95a5a6; font-weight:300; font-size:14px;"),
            p("Click 'Add to Trip' button on map markers to add venues", style = "color:#bdc3c7; font-size:12px; font-weight:300;")
          )
        )
      }

      venue_items <- lapply(seq_len(nrow(selected)), function(i) {
        venue <- selected[i, ]
        venue_id <- URLencode(venue$name, reserved = TRUE)
        border_color <- if (grepl("Bar", venue$category, ignore.case = TRUE)) {
          "#9b59b6"
        } else if (grepl("Cafe|Coffee|Brunch|Bakery", venue$category, ignore.case = TRUE)) {
          "#f39c12"
        } else if (grepl("Milk Tea|Juice|Drink", venue$category, ignore.case = TRUE)) {
          "#e91e63"
        } else {
          "#3498db"
        }

        div(
          `data-venue-name` = venue$name,
          class = "info-box sortable-item",
          style = sprintf("margin-bottom:12px; border-left:5px solid %s; border-top:none; border-right:none; border-bottom:1px solid #e0e0e0; padding:12px 8px 12px 12px; background:#ffffff; position:relative; cursor:move; box-shadow:-2px 0 0 %s inset;", border_color, border_color),
          tags$button(
            class = "btn btn-link",
            style = "position:absolute; top:8px; right:8px; padding:4px 8px; background:transparent; color:#95a5a6; border:none; cursor:pointer; font-size:16px;",
            onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority:'event'})", ns("remove_venue"), venue_id),
            icon("times")
          ),
          tags$span(
            style = sprintf("position:absolute; top:8px; left:8px; width:8px; height:8px; border-radius:50%%; background:%s;", border_color)
          ),
          h4(
            style = "margin:0 30px 8px 0; padding-left:12px; font-size:15px; font-weight:400; color:#2c3e50;",
            venue$name,
            tags$span(style = "display:block; color:#95a5a6; font-size:12px; margin-top:4px; font-weight:300;", sprintf("★ %.1f", venue$rating))
          ),
          p(
            style = "margin:6px 0; padding-left:12px; font-size:12px; color:#7f8c8d; font-weight:300;",
            tags$span(venue$category),
            tags$span(style = "margin-left:15px; color:#2c3e50;", venue$price_level),
            if (!is.null(venue$distance) && venue$distance > 0) tags$span(style = "margin-left:15px;", sprintf("%.1f km", venue$distance))
          ),
          p(
            style = "font-size:11px; color:#95a5a6; margin:4px 0; padding-left:12px; font-weight:300;",
            substr(venue$address, 1, 40),
            if (nchar(venue$address) > 40) "..." else ""
          ),
          if (!is.na(venue$description) && nchar(venue$description) > 0) {
            p(
              style = "font-size:10px; color:#bdc3c7; font-style:italic; margin:4px 0 0 0; padding-left:12px; font-weight:300;",
              substr(venue$description, 1, 60),
              if (nchar(venue$description) > 60) "..." else ""
            )
          }
        )
      })

      div(
        id = ns("sortable-venues-container"),
        class = "sortable-list route-sortable",
        venue_items
      )
    })

    observeEvent(input$venue_order, {
      ord <- input$venue_order
      if (!is.null(ord) && length(ord) > 0) {
        current <- selected_venues()
        reordered <- current[match(ord, current$name), ]
        reordered <- reordered[!is.na(reordered$name), ]
        if (nrow(reordered) > 0) selected_venues(reordered)
      }
    }, ignoreInit = TRUE)

    observeEvent(input$remove_venue, {
      remove_name <- URLdecode(input$remove_venue)
      current <- selected_venues()
      if (nrow(current) > 0) {
        selected_venues(current %>% filter(name != remove_name))
      }
    })

    observeEvent(input$add_to_trip, {
      venue_name <- URLdecode(input$add_to_trip)
      recs <- recommendations()
      if (nrow(recs) == 0) return()

      venue <- recs %>% filter(name == venue_name) %>% slice(1)
      if (nrow(venue) == 0) return()

      current <- selected_venues()
      if (nrow(current) == 0) {
        selected_venues(venue)
      } else if (!venue_name %in% current$name) {
        selected_venues(rbind(current, venue))
      }
      showNotification(sprintf("%s added to trip!", venue_name), type = "message", duration = 3)
    })

    output$map <- renderLeaflet({
      loc <- user_location()
      map <- leaflet() %>% addProviderTiles(providers$CartoDB.Positron)
      if (!is.null(loc)) {
        map <- map %>% setView(lng = loc$lng, lat = loc$lat, zoom = 15)
      } else {
        map <- map %>% setView(lng = 144.9631, lat = -37.8136, zoom = 14)
      }
      if (!is.null(loc)) {
        map <- map %>% addAwesomeMarkers(
          lng = loc$lng,
          lat = loc$lat,
          icon = awesomeIcons(icon = "user", iconColor = "white", library = "fa", markerColor = "blue"),
          popup = "<strong>Your Location</strong>",
          label = "You are here",
          layerId = "user_location",
          group = "user_location"
        )
      }
      legend_html <- htmltools::HTML("
        <div class='route-legend'>
          <div class='route-legend-item'><img src='Restaurant.png' alt='Restaurant icon'/><span>Restaurant</span></div>
          <div class='route-legend-item'><img src='Bar.png' alt='Bar icon'/><span>Bar</span></div>
          <div class='route-legend-item'><img src='Cafe.png' alt='Cafe icon'/><span>Cafe</span></div>
          <div class='route-legend-item'><img src='Milktea.png' alt='Drinks icon'/><span>Drinks</span></div>
        </div>
      ")

      map %>% addLayersControl(
        overlayGroups = c("recommendations", "selected"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
        addControl(legend_html, position = "topright", layerId = "route-legend")
    })

    observe({
      recs <- recommendations()
      selected <- selected_venues()
      if (nrow(selected) > 0 && nrow(recs) > 0) {
        recs <- recs %>% filter(!name %in% selected$name)
      }

      proxy <- leafletProxy(ns("map"), session = session) %>%
        clearGroup("recommendations") %>%
        clearGroup("selected") %>%
        clearShapes()

      if (nrow(recs) > 0) {
        recs$popup_content <- sapply(seq_len(nrow(recs)), function(i) {
          venue_name <- recs$name[i]
          venue_name_encoded <- URLencode(venue_name, reserved = TRUE)
          sprintf(
            "<div style='min-width:200px; font-family:-apple-system,BlinkMacSystemFont,sans-serif;'>
              <strong style='font-size:14px; color:#2c3e50;'>%s</strong><br/>
              <span style='color:#7f8c8d; font-size:12px;'>%s</span><br/>
              <span style='color:#34495e; font-size:12px;'>Rating: %.1f</span><br/>
              <span style='color:#34495e; font-size:12px;'>Price: <strong style='color:#27ae60;'>%s</strong></span><br/>
              <span style='color:#95a5a6; font-size:11px;'>%s</span><br/>
              <button onclick=\"Shiny.setInputValue('%s','%s',{priority:'event'})\" style='margin-top:10px; padding:8px 15px; background-color:#3498db; color:white; border:none; border-radius:0; cursor:pointer; font-weight:300; width:100%%; letter-spacing:0.5px;'>ADD TO TRIP</button>
            </div>",
            venue_name,
            recs$category[i],
            recs$rating[i],
            recs$price_level[i],
            substr(recs$address[i], 1, 50),
            ns("add_to_trip"),
            venue_name_encoded
          )
        })

        recs$icon_file <- sapply(recs$category, category_icon_name, selected = FALSE)
        rec_icons <- icons(
          iconUrl = recs$icon_file,
          iconWidth = 40,
          iconHeight = 55,
          iconAnchorX = 20,
          iconAnchorY = 55,
          popupAnchorX = 1,
          popupAnchorY = -55
        )

        proxy <- proxy %>%
          addMarkers(
            data = recs,
            lng = ~lon,
            lat = ~lat,
            icon = rec_icons,
            popup = ~popup_content,
            label = ~name,
            group = "recommendations",
            layerId = ~name,
            clusterOptions = markerClusterOptions(
              showCoverageOnHover = TRUE,
              zoomToBoundsOnClick = TRUE,
              spiderfyOnMaxZoom = TRUE,
              removeOutsideVisibleBounds = TRUE,
              maxClusterRadius = 80
            )
          )
      }

      if (nrow(selected) > 0) {
        selected_icons <- icons(
          iconUrl = sapply(selected$category, category_icon_name, selected = TRUE),
          iconWidth = 40,
          iconHeight = 55,
          iconAnchorX = 20,
          iconAnchorY = 55,
          popupAnchorX = 1,
          popupAnchorY = -55
        )

        proxy <- proxy %>%
          addMarkers(
            data = selected,
            lng = ~lon,
            lat = ~lat,
            icon = selected_icons,
            popup = ~sprintf(
              "<div style='font-family:-apple-system,BlinkMacSystemFont,sans-serif;'>
                <strong style='color:#e74c3c;'>SELECTED: %s</strong><br/>
                <span style='color:#7f8c8d; font-size:12px;'>%s</span><br/>
                <span style='color:#34495e; font-size:12px;'>Rating: %.1f</span><br/>
                <span style='color:#34495e; font-size:12px;'>Price: <strong style='color:#27ae60;'>%s</strong></span>
              </div>",
              name, category, rating, price_level
            ),
            label = ~paste("SELECTED:", name),
            group = "selected",
            layerId = ~name
          )

        if (nrow(selected) > 1) {
          route_info <- route_calculate_route(selected)
          for (i in 1:(nrow(selected) - 1)) {
            if (!is.null(route_info$route_geometries[[i]])) {
              geometry <- route_info$route_geometries[[i]]
              proxy <- proxy %>%
                addPolylines(
                  lng = geometry[, 1],
                  lat = geometry[, 2],
                  color = "#f59e0b",
                  weight = 4,
                  opacity = 0.8,
                  popup = sprintf(
                    "Distance: %.2f km<br/>Drive: %.1f min<br/>Walk: %.1f min",
                    route_info$route_details$distance_km[i],
                    route_info$route_details$drive_time_min[i],
                    route_info$route_details$walk_time_min[i]
                  )
                )
            } else {
              proxy <- proxy %>%
                addPolylines(
                  lng = c(selected$lon[i], selected$lon[i + 1]),
                  lat = c(selected$lat[i], selected$lat[i + 1]),
                  color = "#f59e0b",
                  weight = 3,
                  opacity = 0.7,
                  dashArray = "5, 5",
                  popup = "Estimated route (API unavailable)"
                )
            }
          }
        }
      }
    })

    observeEvent(input$optimize_route, {
      selected <- selected_venues()
      if (nrow(selected) < 2) {
        showNotification("Please select at least 2 venues to optimize route!", type = "warning", duration = 3)
        return()
      }

      optimized <- selected[1, ]
      remaining <- selected[-1, ]

      while (nrow(remaining) > 0) {
        current <- optimized[nrow(optimized), ]
        distances <- sapply(1:nrow(remaining), function(i) {
          distHaversine(c(current$lon, current$lat), c(remaining$lon[i], remaining$lat[i])) / 1000
        })
        nearest_idx <- which.min(distances)
        optimized <- rbind(optimized, remaining[nearest_idx, ])
        remaining <- remaining[-nearest_idx, ]
      }

      selected_venues(optimized)
      showNotification("Route optimized! Venues reordered for shortest path.", type = "message", duration = 3)
    })

    output$route_table <- renderDT({
      selected <- selected_venues()
      if (nrow(selected) < 2) return(datatable(data.frame(Message = "Select at least two venues to build a route."), options = list(dom = 't')))

      route_info <- route_calculate_route(selected)
      datatable(
        route_info$route_details,
        rownames = FALSE,
        options = list(
          pageLength = 5,
          lengthChange = FALSE,
          searching = FALSE,
          ordering = FALSE,
          info = FALSE
        )
      )
    })

  })
}
