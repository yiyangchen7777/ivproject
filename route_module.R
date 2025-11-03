route_is_open_single <- function(hours_str) {
  if (is.null(hours_str) || is.na(hours_str) || !nzchar(hours_str)) return(NA)

  if (grepl("24", hours_str, ignore.case = TRUE) && grepl("hour", hours_str, ignore.case = TRUE)) {
    return(TRUE)
  }

  today_full <- weekdays(Sys.Date())
  pattern <- paste0("(?i)", today_full, ":[^|]+")
  today_segment <- regmatches(hours_str, regexpr(pattern, hours_str, perl = TRUE))

  if (!length(today_segment) || grepl("Closed", today_segment, ignore.case = TRUE)) {
    if (exists("is_open_today_now", mode = "function")) {
      return(tryCatch(is_open_today_now(hours_str), error = function(...) NA))
    }
    return(FALSE)
  }

  time_pattern <- "\\d{1,2}:\\d{2}\\s*(AM|PM)?\\s*[-–]\\s*\\d{1,2}:\\d{2}\\s*(AM|PM)?"
  time_match <- regmatches(today_segment, regexpr(time_pattern, today_segment, perl = TRUE, ignore.case = TRUE))

  if (!length(time_match)) {
    if (exists("is_open_today_now", mode = "function")) {
      return(tryCatch(is_open_today_now(hours_str), error = function(...) NA))
    }
    return(NA)
  }

  to_24 <- function(txt) {
    txt <- trimws(txt)
    if (grepl("AM|PM", txt, ignore.case = TRUE)) {
      tryCatch(
        format(lubridate::parse_date_time(txt, orders = "I:M p"), "%H:%M"),
        error = function(...) NA_character_
      )
    } else {
      parts <- strsplit(txt, ":", fixed = TRUE)[[1]]
      if (length(parts) < 2) return(NA_character_)
      h <- as.numeric(parts[1])
      m <- as.numeric(parts[2])
      if (is.na(h) || is.na(m)) return(NA_character_)
      if (h < 6) h <- h + 12
      sprintf("%02d:%02d", h, m)
    }
  }

  bounds <- strsplit(gsub("\\s", "", time_match), "[-–]")[[1]]
  if (length(bounds) < 2) return(NA)

  open_time <- to_24(bounds[1])
  close_time <- to_24(bounds[2])
  if (is.na(open_time) || is.na(close_time)) return(NA)

  now_time <- format(Sys.time(), "%H:%M")

  if (close_time < open_time) {
    return(now_time >= open_time || now_time <= close_time)
  }
  now_time >= open_time && now_time <= close_time
}

route_is_open_now <- function(hours_vec) {
  if (length(hours_vec) == 0) return(logical(0))
  vapply(hours_vec, route_is_open_single, logical(1))
}

load_route_data <- function() {
  cafes <- read.csv("cafe.csv", stringsAsFactors = FALSE)
  bars <- read.csv("bars.csv", stringsAsFactors = FALSE)
  drinks <- read.csv("milk_juice.csv", stringsAsFactors = FALSE)
  restaurants <- read.csv("restaurant.csv", stringsAsFactors = FALSE)

  cafes$opening_hours <- if ("opening_hours" %in% names(cafes)) cafes$opening_hours else if ("openinghour" %in% names(cafes)) cafes$openinghour else NA_character_
  bars$opening_hours <- if ("opening_hours" %in% names(bars)) bars$opening_hours else if ("openinghour" %in% names(bars)) bars$openinghour else NA_character_
  drinks$opening_hours <- if ("opening_hours" %in% names(drinks)) drinks$opening_hours else if ("openinghour" %in% names(drinks)) drinks$openinghour else NA_character_
  restaurants$opening_hours <- if ("opening_hours" %in% names(restaurants)) restaurants$opening_hours else if ("openinghour" %in% names(restaurants)) restaurants$openinghour else NA_character_

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
    ) %>%
    mutate(
      opening_hours = ifelse(is.na(opening_hours) | opening_hours == "", NA_character_, opening_hours),
      open_now = route_is_open_now(opening_hours),
      category_group = dplyr::case_when(
        grepl("Bar", category, ignore.case = TRUE) ~ "Bar",
        grepl("Cafe|Coffee|Brunch|Bakery", category, ignore.case = TRUE) ~ "Cafe/Brunch",
        grepl("Milk Tea|Juice|Drink", category, ignore.case = TRUE) ~ "Drinks",
        TRUE ~ "Restaurant"
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
    grepl("Low", budget, ignore.case = TRUE) ~ 1,
    grepl("Medium", budget, ignore.case = TRUE) ~ 2,
    grepl("High", budget, ignore.case = TRUE) ~ 3,
    grepl("Luxury", budget, ignore.case = TRUE) ~ 4,
    TRUE ~ 2
  )

  time_categories <- switch(
    meal_time,
    "Breakfast (7-10 AM)" = c("Cafe/Brunch", "Drinks"),
    "Lunch (12-2 PM)"    = c("Restaurant", "Cafe/Brunch", "Drinks", "Bar"),
    "Dinner (6-9 PM)"    = c("Restaurant", "Bar"),
    "Late Night (9 PM+)" = c("Bar", "Drinks"),
    c("Restaurant", "Cafe/Brunch", "Bar", "Drinks")
  )

  search_active <- !is.null(search_text) && nzchar(search_text)

  filtered <- data %>%
    filter(price_numeric <= budget_filter + 1)

  if (search_active) {
    filtered <- filtered %>%
      filter(
        grepl(search_text, name, ignore.case = TRUE) |
          grepl(search_text, category, ignore.case = TRUE) |
          grepl(search_text, address, ignore.case = TRUE)
      )
  } else {
    filtered <- filtered %>%
      filter(category_group %in% time_categories)
  }

  if (nrow(filtered) == 0) {
    return(filtered)
  }

  if (isTRUE(use_location) && !is.null(user_coords)) {
    center_lat <- user_coords$lat
    center_lon <- user_coords$lng

    filtered <- filtered %>%
      mutate(
        distance = (distHaversine(cbind(lon, lat), c(center_lon, center_lat)) / 1000) * 1.3
      )
  } else {
    filtered <- filtered %>%
      mutate(distance = 0)
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
  # Validate coordinates
  if (any(is.na(c(from_lon, from_lat, to_lon, to_lat)))) return(NULL)
  if (any(!is.numeric(c(from_lon, from_lat, to_lon, to_lat)))) return(NULL)
  # Validate coordinate range (Melbourne approx: lon 144.5-145.5, lat -38.5 to -37.5)
  if (from_lat < -90 || from_lat > 90 || to_lat < -90 || to_lat > 90) return(NULL)
  if (from_lon < -180 || from_lon > 180 || to_lon < -180 || to_lon > 180) return(NULL)
  
  tryCatch({
    url <- sprintf(
      "http://router.project-osrm.org/route/v1/driving/%f,%f;%f,%f?overview=full&geometries=geojson",
      from_lon, from_lat, to_lon, to_lat
    )
    response <- jsonlite::fromJSON(url)
    if (response$code == "Ok" && length(response$routes) > 0 && nrow(response$routes) > 0) {
      route <- response$routes[1, ]
      if (!is.null(route$geometry) && !is.null(route$geometry$coordinates)) {
        return(list(
          distance = route$distance / 1000,
          duration = route$duration / 60,
          geometry = route$geometry$coordinates[[1]]
        ))
      }
    }
    NULL
  }, error = function(e) {
    # Fail silently and return NULL
    NULL
  })
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
      
      # Ensure coordinate fields exist and are valid
      if (!"lon" %in% names(from) || !"lat" %in% names(from) ||
          !"lon" %in% names(to) || !"lat" %in% names(to)) {
        # Skip invalid venues but still append to route_details with placeholders
        route_details <- rbind(
          route_details,
          data.frame(
            from = from$name,
            to = to$name,
            distance_km = 0,
            walk_time_min = 0,
            drive_time_min = 0
          )
        )
        route_geometries[[i]] <- NULL
        next
      }
      
      from_lon <- as.numeric(from$lon)
      from_lat <- as.numeric(from$lat)
      to_lon <- as.numeric(to$lon)
      to_lat <- as.numeric(to$lat)
      
      # Validate that coordinates are not NA and fall within bounds
      if (any(is.na(c(from_lon, from_lat, to_lon, to_lat)))) {
        route_details <- rbind(
          route_details,
          data.frame(
            from = from$name,
            to = to$name,
            distance_km = 0,
            walk_time_min = 0,
            drive_time_min = 0
          )
        )
        route_geometries[[i]] <- NULL
        next
      }
      
      # Validate coordinate range
      if (from_lat < -90 || from_lat > 90 || to_lat < -90 || to_lat > 90 ||
          from_lon < -180 || from_lon > 180 || to_lon < -180 || to_lon > 180) {
        route_details <- rbind(
          route_details,
          data.frame(
            from = from$name,
            to = to$name,
            distance_km = 0,
            walk_time_min = 0,
            drive_time_min = 0
          )
        )
        route_geometries[[i]] <- NULL
        next
      }

      osrm_route <- route_get_route_from_osrm(from_lon, from_lat, to_lon, to_lat)

      if (!is.null(osrm_route)) {
        distance <- osrm_route$distance
        drive_time <- osrm_route$duration
        walk_time <- (distance / 5) * 60
        route_geometries[[i]] <- osrm_route$geometry
      } else {
        # Use an estimated distance
        straight_distance <- distHaversine(c(from_lon, from_lat), c(to_lon, to_lat)) / 1000
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
          drive_time_min = round(drive_time, 1),
          stringsAsFactors = FALSE
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
      tags$script(HTML(sprintf("
        Shiny.addCustomMessageHandler('route_refresh_map', function(message){
          setTimeout(function(){
            Shiny.setInputValue('%s', Date.now(), {priority:'event'});
            var mapWidget = HTMLWidgets.find('#%s');
            if (mapWidget && mapWidget.length > 0) {
              mapWidget.forEach(function(w){
                if (w && w.instance && w.instance.invalidateSize) {
                  w.instance.invalidateSize();
                }
              });
            }
          }, 300);
        });
      ", ns("map_refresh"), ns("map")))),
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
        .route-box .box-body { overflow:hidden !important; }
        .route-box[style*='flex'] .box-body { display:flex !important; flex-direction:column !important; }
        .route-box[style*='flex'] .box-body > * { flex-shrink:0; }
        .route-box[style*='flex'] .box-body > div[style*='flex:1'] { flex:1 !important; min-height:0; }
        .route-sortable .sortable-item { transition:all 0.2s ease; }
        .route-sortable .sortable-item:hover { box-shadow:0 2px 8px rgba(0,0,0,0.1); }
        .route-sortable .sortable-ghost { opacity:0.4; background-color:#ecf0f1; }
        .route-sortable .sortable-drag { opacity:0.8; box-shadow:0 4px 12px rgba(0,0,0,0.15); }
        .route-layout { display:flex; gap:24px; align-items:flex-start; }
        .route-layout__sidebar { flex:0 0 280px; }
        .route-layout__main { flex:1 1 auto; }
        @media (max-width:1200px) {
          .route-layout { flex-direction:column; }
          .route-layout__sidebar { flex:1 1 auto; }
        }
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
        .route-see-details {
          margin-left:auto;
          font-size:12px;
          color:#3478f6;
          text-decoration:none;
          font-weight:500;
        }
        .route-see-details:hover {
          text-decoration:underline;
          color:#265ed2;
        }
      "))
    ),
    div(
      class = "route-layout",
      div(
        class = "route-layout__sidebar route-sidebar",
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
      ),
      div(
        class = "route-layout__main",
        fluidRow(
          shinydashboard::box(
            title = "SELECTED VENUES",
            width = 5,
            solidHeader = FALSE,
            status = "primary",
            class = "route-box",
            style = "max-height:600px; overflow:hidden; display:flex; flex-direction:column;",
            uiOutput(ns("trip_summary")),
            div(
              style = "display:flex; justify-content:space-between; align-items:center; padding:8px 10px; margin-bottom:10px; flex-shrink:0; gap:12px; flex-wrap:wrap;",
              uiOutput(ns("route_stats")),
              actionButton(
                ns("optimize_route"),
                "OPTIMIZE ROUTE",
                icon = icon("route"),
                class = "route-optimize-btn",
                style = "padding:10px 24px; font-size:14px; letter-spacing:0.5px; border-radius:10px; background:#3478f6; border:none; color:white; box-shadow:0 4px 10px rgba(52,120,246,0.25); transition:all .3s ease; flex-shrink:0;"
              )
            ),
            div(style = "flex:1; min-height:0; overflow-y:auto;", class = "route-sortable",
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
          style = "margin-top:20px;",
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

route_module_server <- function(id, map_user_location = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    message_id <- sprintf("route_get_location_%s", id)

    all_venues <- reactiveVal(load_route_data())
    recommendations <- reactiveVal(data.frame())
    selected_venues <- reactiveVal(data.frame())
    user_location <- reactiveVal(NULL)
    map_refresh_trigger <- reactiveVal(0)

    if (!is.null(map_user_location)) {
      observeEvent(map_user_location(), {
        loc <- map_user_location()
        if (is.null(loc)) {
          user_location(NULL)
          return()
        }
        lat_val <- loc$lat
        if (is.null(lat_val) && !is.null(loc$latitude)) lat_val <- loc$latitude
        lng_val <- loc$lon
        if (is.null(lng_val) && !is.null(loc$lng)) lng_val <- loc$lng
        lat_num <- suppressWarnings(as.numeric(lat_val))
        lng_num <- suppressWarnings(as.numeric(lng_val))
        if (is.na(lat_num) || is.na(lng_num)) return()
        name_val <- loc$name
        if (is.null(name_val) || !nzchar(name_val)) {
          name_val <- "Map Page Location"
        }
        ts_val <- loc$ts
        if (is.null(ts_val)) ts_val <- Sys.time()
        user_location(list(
          name = name_val,
          lat = lat_num,
          lng = lng_num,
          ts = ts_val
        ))
      }, ignoreNULL = FALSE)
    }

    category_icon_name <- function(cat, selected = FALSE) {
      cat <- ifelse(is.na(cat), "", cat)
      # Use a dedicated start icon for the User Location
      if (grepl("User Location", cat, ignore.case = TRUE)) {
        # Use locate_icon.png when available; otherwise fall back to the default icon
        if (file.exists("www/locate_icon.png")) {
          return("locate_icon.png")
        } else {
          # If locate_icon.png is missing return a placeholder so a custom icon can be added later
          return("locate_icon.png")  # Try to use this name even if the file is missing
        }
      }
      
      base <- "Restaurant"
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

    format_price <- function(value) {
      if (is.null(value) || length(value) == 0 || is.na(value) || value %in% c("", "N/A")) {
        "No Price Info"
      } else {
        value
      }
    }

    format_open_status <- function(open_now, opening_hours) {
      if (is.null(opening_hours) || length(opening_hours) == 0 || is.na(opening_hours) || !nzchar(opening_hours)) {
        "No Opening Hours Info"
      } else if (isTRUE(open_now)) {
        "🟢 <i>Open</i>"
      } else if (identical(open_now, FALSE)) {
        "🔴 <i>Closed</i>"
      } else {
        "No Opening Hours Info"
      }
    }

    price_level_to_budget <- function(price_level) {
      switch(
        price_level,
        "$" = "Low ($)",
        "$$" = "Medium ($$)",
        "$$$" = "High ($$$)",
        "$$$$" = "Luxury ($$$$)",
        "N/A" = "Medium ($$)",
        NULL
      )
    }

    category_group_to_mealtime <- function(group) {
      switch(
        group,
        "Cafe/Brunch" = "Breakfast (7-10 AM)",
        "Bar" = "Late Night (9 PM+)",
        "Drinks" = "Anytime",
        "Restaurant" = "Dinner (6-9 PM)",
        "Anytime"
      )
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
        # Build popup content that includes the Add to Route button
        popup_content <- sprintf(
          "<div style='min-width:200px; font-family:-apple-system,BlinkMacSystemFont,sans-serif;'>
            <strong style='font-size:14px; color:#2c3e50;'>📍 Your Location</strong><br/>
            <span style='color:#7f8c8d; font-size:12px;'>Lat: %.4f, Lng: %.4f</span><br/>
            <button onclick=\"Shiny.setInputValue('%s','user_location',{priority:'event'})\" style='margin-top:10px; padding:8px 15px; background-color:#3478f6; color:white; border:none; border-radius:6px; cursor:pointer; font-weight:500; width:100%%; letter-spacing:0.5px;'>➕ Add to Route (Start Point)</button>
          </div>",
          loc$lat,
          loc$lng,
          ns("add_user_location")
        )
        
        proxy %>%
          addAwesomeMarkers(
            lng = loc$lng,
            lat = loc$lat,
            icon = awesomeIcons(icon = "user", iconColor = "white", library = "fa", markerColor = "blue"),
            popup = popup_content,
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

    observeEvent(input$search_text, {
      text <- trimws(input$search_text)
      if (!nzchar(text)) return()

      venues <- all_venues()
      if (nrow(venues) == 0) return()

      exact_matches <- venues %>%
        filter(tolower(name) == tolower(text))

      candidate <- if (nrow(exact_matches) == 1) {
        exact_matches
      } else {
        partial_matches <- venues %>%
          filter(grepl(text, name, ignore.case = TRUE))
        if (nrow(partial_matches) == 1) partial_matches else NULL
      }

      if (is.null(candidate) || nrow(candidate) == 0) return()

      venue <- candidate[1, ]
      budget_choice <- price_level_to_budget(venue$price_level)
      meal_choice <- category_group_to_mealtime(venue$category_group)

      if (!is.null(budget_choice)) {
        updateSelectInput(session, "budget", selected = budget_choice)
      }
      if (!is.null(meal_choice)) {
        updateSelectInput(session, "meal_time", selected = meal_choice)
      }
    }, ignoreNULL = TRUE)

    observeEvent(input$clear_selection, {
      selected_venues(data.frame())
      showNotification("Selection cleared!", type = "warning", duration = 2)
    })

    output$trip_summary <- renderUI({
      selected <- selected_venues()
      if (nrow(selected) == 0) return(NULL)

      cafes <- sum(selected$category_group == "Cafe/Brunch", na.rm = TRUE)
      bars <- sum(selected$category_group == "Bar", na.rm = TRUE)
      drinks <- sum(selected$category_group == "Drinks", na.rm = TRUE)
      restaurants <- sum(selected$category_group == "Restaurant", na.rm = TRUE)

      # Calculate route information if there are 2+ venues
      route_info <- NULL
      if (nrow(selected) >= 2) {
        # Filter to ensure valid coordinates
        selected_valid <- selected %>% 
          filter(!is.na(lat) & !is.na(lon) & !is.na(lon) & !is.na(lat))
        if (nrow(selected_valid) >= 2) {
          route_info <- route_calculate_route(selected_valid)
        }
      }

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

    output$route_stats <- renderUI({
      selected <- selected_venues()
      
      # Calculate route information if there are 2+ venues
      route_info <- NULL
      if (nrow(selected) >= 2) {
        # Filter to ensure valid coordinates
        selected_valid <- selected %>% 
          filter(!is.na(lat) & !is.na(lon) & !is.na(lon) & !is.na(lat))
        if (nrow(selected_valid) >= 2) {
          route_info <- route_calculate_route(selected_valid)
        }
      }
      
      if (is.null(route_info) || route_info$total_distance == 0) {
        return(NULL)
      }
      
      div(
        style = "display: flex; align-items: center; gap: 16px; flex-wrap: wrap;",
        div(
          style = "display: flex; align-items: center; gap: 6px;",
          tags$span(style = "font-size: 16px;", "📏"),
          tags$span(style = "color: #34495e; font-weight: 500; font-size: 13px;", sprintf("%.2f km", route_info$total_distance))
        ),
        div(
          style = "display: flex; align-items: center; gap: 6px;",
          tags$span(style = "font-size: 16px;", "🚶"),
          tags$span(style = "color: #34495e; font-weight: 500; font-size: 13px;", sprintf("%.1f min", route_info$total_walk_time))
        ),
        div(
          style = "display: flex; align-items: center; gap: 6px;",
          tags$span(style = "font-size: 16px;", "🚗"),
          tags$span(style = "color: #34495e; font-weight: 500; font-size: 13px;", sprintf("%.1f min", route_info$total_drive_time))
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
        venue_group <- venue$category_group
        if (is.null(venue_group) || length(venue_group) == 0 || is.na(venue_group)) {
          venue_group <- "Restaurant"
        }
        border_color <- if (venue_group == "Bar") {
          "#9b59b6"
        } else if (venue_group == "Cafe/Brunch") {
          "#f39c12"
        } else if (venue_group == "Drinks") {
          "#e91e63"
        } else if (venue_group == "User Location") {
          "#3498db"  # Use blue for the user location
        } else {
          "#3498db"
        }
        price_display <- format_price(venue$price_level)
        address_raw <- if ("address" %in% names(venue) && !is.null(venue$address) && !is.na(venue$address)) {
          as.character(venue$address)
        } else {
          ""
        }
        display_address <- if (nzchar(address_raw)) {
          sub(",\\s*Australia\\s*$", "", address_raw)
        } else {
          ""
        }
        status_display <- format_open_status(venue$open_now, venue$opening_hours)

        details_link <- tags$a(
          "See Details",
          href = "#",
          class = "route-see-details",
          onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority:'event'})", ns("see_details"), venue_id)
        )

        # Show a simplified card for the User Location (no details)
        is_user_location <- !is.null(venue_group) && venue_group == "User Location"
        
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
            if (is_user_location) {
              tags$span(style = "font-size: 16px; margin-right: 6px;", "📍")
            } else {
              NULL
            },
            venue$name,
            if (!is_user_location) {
              tags$span(
                style = "display:block; color:#95a5a6; font-size:12px; margin-top:4px; font-weight:300;",
                sprintf("★ %.1f", if ("rating" %in% names(venue) && !is.null(venue$rating) && !is.na(venue$rating)) venue$rating else 0)
              )
            } else {
              NULL
            }
          ),
          # Only non User Location entries display details
          if (!is_user_location) {
            tagList(
              p(
                style = "margin:6px 0; padding-left:12px; font-size:12px; color:#7f8c8d; font-weight:300; display:flex; align-items:center; gap:15px;",
                tags$span(if ("category" %in% names(venue) && !is.null(venue$category) && !is.na(venue$category)) as.character(venue$category) else ""),
                tags$span(style = "color:#2c3e50;", price_display),
                if ("distance" %in% names(venue) && !is.null(venue$distance) && !is.na(venue$distance) && is.numeric(venue$distance) && venue$distance > 0) {
                  tags$span(sprintf("%.1f km", venue$distance))
                } else {
                  NULL
                },
                details_link
              ),
              p(
                style = "font-size:11px; color:#7f8c8d; margin:4px 0; padding-left:12px; font-weight:300;",
                tags$span(style = "font-weight:500; color:#2c3e50;", "Status: "),
                HTML(status_display)
              ),
              p(
                style = "font-size:11px; color:#95a5a6; margin:4px 0; padding-left:12px; font-weight:300;",
                if (!is.null(display_address) && !is.na(display_address) && nzchar(display_address)) {
                  paste0(substr(display_address, 1, 40), if (nchar(display_address) > 40) "..." else "")
                } else {
                  ""
                }
              ),
              if (!is.null(venue$description) && !is.na(venue$description) && nzchar(as.character(venue$description))) {
                p(
                  style = "font-size:10px; color:#bdc3c7; font-style:italic; margin:4px 0 0 0; padding-left:12px; font-weight:300;",
                  paste0(substr(venue$description, 1, 60), if (nchar(venue$description) > 60) "..." else "")
                )
              } else {
                NULL
              }
            )
          } else {
            # User Location shows only a starting-point hint
            p(
              style = "margin:6px 0; padding-left:12px; font-size:12px; color:#7f8c8d; font-weight:300; font-style:italic;",
              "🚩 Starting Point"
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

    observeEvent(input$add_user_location, {
      loc <- user_location()
      if (is.null(loc)) {
        showNotification("Please set your location first!", type = "warning", duration = 2)
        return()
      }
      
      # Build a pseudo-venue entry for the user location
      # Ensure coordinates stay within valid ranges (lat -90–90, lon -180–180)
      # Note: upstream app.R sends fields named lat and lon (not lng)
      lat_val <- as.numeric(loc$lat)
      lon_val <- as.numeric(loc$lon %||% loc$lng)  # Accept both lon and lng
      
      # Validate coordinates
      if (is.na(lat_val) || is.na(lon_val)) {
        showNotification("Invalid location coordinates!", type = "error", duration = 2)
        return()
      }
      
      # Validate and adjust coordinates (Melbourne approx lat -38.5 to -37.5, lon 144.5 to 145.5)
      # Swap values when the coordinates look outside Melbourne's range
      if ((lat_val > 90 || lat_val < -90) || 
          (abs(lat_val) < abs(lon_val) && abs(lon_val) < 90)) {
        # Swap lat/lon if lat is invalid or the pair appears reversed
        temp <- lat_val
        lat_val <- lon_val
        lon_val <- temp
      }
      
      # Re-validate the coordinate range
      if (lat_val < -90 || lat_val > 90 || lon_val < -180 || lon_val > 180) {
        showNotification("Invalid location coordinates range!", type = "error", duration = 2)
        return()
      }
      
      user_venue <- data.frame(
        name = loc$name %||% "Your Location",
        lat = lat_val,
        lon = lon_val,
        category = "User Location",
        category_group = "User Location",
        rating = NA_real_,
        price_level = "N/A",
        price_numeric = 2,
        address = sprintf("Lat: %.4f, Lng: %.4f", lat_val, lon_val),
        phone = NA_character_,
        website = NA_character_,
        opening_hours = NA_character_,
        open_now = NA,
        description = NA_character_,
        stringsAsFactors = FALSE
      )
      
      current <- selected_venues()
      
      # Check whether the user location already exists (by coordinates)
      if (nrow(current) > 0) {
        # Detect an existing user location by name or coordinates
        has_user_location <- any(
          current$name == user_venue$name | 
          (abs(current$lat - user_venue$lat) < 0.0001 & abs(current$lon - user_venue$lon) < 0.0001),
          na.rm = TRUE
        )
        
        if (has_user_location) {
          showNotification("Your location is already in the route!", type = "warning", duration = 2)
          return()
        }
        
        # Insert it at the first position
        selected_venues(bind_rows(user_venue, current))
      } else {
        # Add directly when the list is empty
        selected_venues(user_venue)
      }
      
      showNotification("Your location added as starting point!", type = "message", duration = 3)
    })

    observeEvent(input$add_from_map, {
      venue_name <- URLdecode(input$add_from_map)
      if (is.null(venue_name) || !nzchar(venue_name)) return()
      
      # Look up the venue in all_venues
      venues <- all_venues()
      
      # Search the combined dataset
      venue <- venues %>% filter(name == venue_name) %>% slice(1)
      
      if (nrow(venue) == 0) {
        showNotification(sprintf("Venue '%s' not found in route data.", venue_name), type = "warning", duration = 2)
        return()
      }
      
      current <- selected_venues()
      
      # Check whether it already exists
      if (venue_name %in% current$name) {
        showNotification(sprintf("%s is already in the route!", venue_name), type = "warning", duration = 2)
        return()
      }
      
      # Append to the list
      if (nrow(current) == 0) {
        selected_venues(venue)
      } else {
        # Use bind_rows instead of rbind so column mismatches are handled automatically
        selected_venues(bind_rows(current, venue))
      }
      
      showNotification(sprintf("%s added to route!", venue_name), type = "message", duration = 3)
    })

    observeEvent(input$add_to_trip, {
      venue_name <- URLdecode(input$add_to_trip)
      recs <- recommendations()
      if (nrow(recs) == 0) return()

      venue <- recs %>% filter(name == venue_name) %>% slice(1)
      if (nrow(venue) == 0) return()

      current <- selected_venues()
      
      # Check whether it already exists
      if (venue_name %in% current$name) {
        showNotification(sprintf("%s is already in the route!", venue_name), type = "warning", duration = 2)
        return()
      }
      
      # Append to the list (use bind_rows instead of rbind)
      if (nrow(current) == 0) {
        selected_venues(venue)
      } else {
        # Use bind_rows instead of rbind so column mismatches are handled automatically
        selected_venues(bind_rows(current, venue))
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

    observeEvent(input$map_refresh, {
      map_refresh_trigger(input$map_refresh)
    }, ignoreInit = TRUE)
    
    observe({
      # Explicitly touch dependencies so observe tracks these reactive values
      recs <- recommendations()
      selected <- selected_venues()
      map_refresh_trigger()  # Trigger map refresh
      
      if (nrow(selected) > 0 && nrow(recs) > 0) {
        recs <- recs %>% filter(!name %in% selected$name)
      }

      proxy <- leafletProxy(ns("map"), session = session) %>%
        clearGroup("recommendations") %>%
        clearGroup("selected") %>%
        clearShapes()

      if (nrow(recs) > 0) {
        recs$price_text <- vapply(recs$price_level, format_price, character(1))
        recs$opening_text <- mapply(
          format_open_status,
          recs$open_now,
          recs$opening_hours,
          USE.NAMES = FALSE
        )
        recs$popup_content <- sapply(seq_len(nrow(recs)), function(i) {
          venue_name <- recs$name[i]
          venue_name_encoded <- URLencode(venue_name, reserved = TRUE)
          sprintf(
            "<div style='min-width:200px; font-family:-apple-system,BlinkMacSystemFont,sans-serif;'>
              <strong style='font-size:14px; color:#2c3e50;'>%s</strong><br/>
              <span style='color:#7f8c8d; font-size:12px;'>%s</span><br/>
              <span style='color:#34495e; font-size:12px;'>Rating: %.1f</span><br/>
              <span style='color:#34495e; font-size:12px;'>Price: <strong style='color:#27ae60;'>%s</strong></span><br/>
              <span style='color:#34495e; font-size:12px;'>Status: %s</span><br/>
              <span style='color:#95a5a6; font-size:11px;'>%s</span><br/>
              <button onclick=\"Shiny.setInputValue('%s','%s',{priority:'event'})\" style='margin-top:10px; padding:8px 15px; background-color:#3498db; color:white; border:none; border-radius:0; cursor:pointer; font-weight:300; width:100%%; letter-spacing:0.5px;'>ADD TO TRIP</button>
            </div>",
            venue_name,
            recs$category[i],
            recs$rating[i],
            recs$price_text[i],
            recs$opening_text[i],
            substr(recs$address[i], 1, 50),
            ns("add_to_trip"),
            venue_name_encoded
          )
        })

        recs$icon_file <- sapply(recs$category_group, category_icon_name, selected = FALSE)
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
        # Filter out venues with missing coordinates
        selected <- selected %>% 
          filter(!is.na(lat) & !is.na(lon) & !is.na(lon) & !is.na(lat))
        
        if (nrow(selected) == 0) {
          return()  # Return immediately when no valid venues remain
        }
        
        # Ensure all required columns exist
        if (!"category_group" %in% names(selected)) {
          selected$category_group <- dplyr::case_when(
            grepl("Bar", selected$category, ignore.case = TRUE) ~ "Bar",
            grepl("Cafe|Coffee|Brunch|Bakery", selected$category, ignore.case = TRUE) ~ "Cafe/Brunch",
            grepl("Milk Tea|Juice|Drink", selected$category, ignore.case = TRUE) ~ "Drinks",
            TRUE ~ "Restaurant"
          )
        }
        
        # Ensure the category column exists
        if (!"category" %in% names(selected)) {
          selected$category <- selected$category_group
        }
        
        # Ensure the price_level column exists
        if (!"price_level" %in% names(selected)) {
          selected$price_level <- "$$"
        }
        
        # Ensure the rating column exists
        if (!"rating" %in% names(selected)) {
          selected$rating <- 3.5
        }
        
        selected$price_text <- vapply(selected$price_level, format_price, character(1))
        selected$opening_text <- mapply(
          format_open_status,
          if ("open_now" %in% names(selected)) selected$open_now else NA,
          if ("opening_hours" %in% names(selected)) selected$opening_hours else NA,
          USE.NAMES = FALSE
        )
        
        selected_icons <- icons(
          iconUrl = sapply(selected$category_group, category_icon_name, selected = TRUE),
          iconWidth = 40,
          iconHeight = 55,
          iconAnchorX = 20,
          iconAnchorY = 55,
          popupAnchorX = 1,
          popupAnchorY = -55
        )

        # Build popup content for each selected venue
        selected$popup_content <- sapply(seq_len(nrow(selected)), function(i) {
          if (selected$category_group[i] == "User Location") {
            sprintf(
              "<div style='font-family:-apple-system,BlinkMacSystemFont,sans-serif;'>
                <strong style='color:#e74c3c;'>SELECTED: %s</strong><br/>
                <span style='color:#7f8c8d; font-size:12px;'>📍 User Location</span>
              </div>",
              selected$name[i]
            )
          } else {
            sprintf(
              "<div style='font-family:-apple-system,BlinkMacSystemFont,sans-serif;'>
                <strong style='color:#e74c3c;'>SELECTED: %s</strong><br/>
                <span style='color:#7f8c8d; font-size:12px;'>%s</span><br/>
                <span style='color:#34495e; font-size:12px;'>Rating: %.1f</span><br/>
                <span style='color:#34495e; font-size:12px;'>Price: <strong style='color:#27ae60;'>%s</strong></span><br/>
                <span style='color:#34495e; font-size:12px;'>Status: %s</span>
              </div>",
              selected$name[i],
              selected$category[i],
              selected$rating[i],
              selected$price_text[i],
              selected$opening_text[i]
            )
          }
        })

        proxy <- proxy %>%
          addMarkers(
            data = selected,
            lng = ~lon,
            lat = ~lat,
            icon = selected_icons,
            popup = ~popup_content,
            label = ~paste("SELECTED:", name),
            group = "selected",
            layerId = ~name
          )

        if (nrow(selected) > 1) {
          route_info <- route_calculate_route(selected)
          
          # Validate route_info before drawing
          if (!is.null(route_info) && !is.null(route_info$route_geometries) && 
              nrow(route_info$route_details) > 0) {
            for (i in 1:(nrow(selected) - 1)) {
              # Ensure the index is valid
              if (i > nrow(route_info$route_details)) next
              
              # Verify coordinate values
              from_lon <- as.numeric(selected$lon[i])
              from_lat <- as.numeric(selected$lat[i])
              to_lon <- as.numeric(selected$lon[i + 1])
              to_lat <- as.numeric(selected$lat[i + 1])
              
              if (any(is.na(c(from_lon, from_lat, to_lon, to_lat)))) next
              
              # Check whether route geometry is available
              if (i <= length(route_info$route_geometries) && 
                  !is.null(route_info$route_geometries[[i]]) && 
                  is.matrix(route_info$route_geometries[[i]]) &&
                  nrow(route_info$route_geometries[[i]]) > 0 &&
                  ncol(route_info$route_geometries[[i]]) >= 2) {
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
                # Fall back to a straight polyline
                proxy <- proxy %>%
                  addPolylines(
                    lng = c(from_lon, to_lon),
                    lat = c(from_lat, to_lat),
                    color = "#f59e0b",
                    weight = 3,
                    opacity = 0.7,
                    dashArray = "5, 5",
                    popup = if (i <= nrow(route_info$route_details)) {
                      sprintf(
                        "Distance: %.2f km<br/>Walk: %.1f min<br/>Drive: %.1f min (Estimated)",
                        route_info$route_details$distance_km[i],
                        route_info$route_details$walk_time_min[i],
                        route_info$route_details$drive_time_min[i]
                      )
                    } else {
                      "Estimated route (API unavailable)"
                    }
                  )
              }
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
