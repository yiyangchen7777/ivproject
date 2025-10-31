# ============================================================================
# Melbourne Restaurant & Drink Shop Recommendation System
# ============================================================================

library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(tidyr)
library(stringr)
library(DT)
library(ggplot2)
library(plotly)
library(geosphere)
library(lubridate)
library(jsonlite)

# ============================================================================
# Data Loading and Preprocessing
# ============================================================================

load_data <- function() {
  # Load all data files
  cafes <- read.csv("cafe_brunch_bakery_desc.csv", stringsAsFactors = FALSE)
  bars <- read.csv("melbourne_cbd_bars.csv", stringsAsFactors = FALSE)
  drinks <- read.csv("milktea_juice_english_clean.csv", stringsAsFactors = FALSE)
  restaurants <- read.csv("restaurant_english_clean_desc.csv", stringsAsFactors = FALSE)
  
  # Standardize column names
  cafes$category <- "Cafe/Brunch"
  bars$category <- "Bar"
  
  # Merge all data
  all_data <- bind_rows(cafes, bars, drinks, restaurants)
  
  # Data cleaning
  all_data <- all_data %>%
    filter(!is.na(lat) & !is.na(lon) & !is.na(name)) %>%
    mutate(
      rating = as.numeric(rating),
      rating = ifelse(is.na(rating), 3.5, rating),
      # Handle price levels, including empty and unknown values
      price_level = case_when(
        is.na(price_level) | price_level == "" ~ "N/A",
        grepl("INEXPENSIVE", price_level, ignore.case = TRUE) ~ "$",
        grepl("MODERATE|moderate", price_level, ignore.case = TRUE) ~ "$$",
        grepl("EXPENSIVE", price_level, ignore.case = TRUE) ~ "$$$",
        grepl("VERY_EXPENSIVE", price_level, ignore.case = TRUE) ~ "$$$$",
        TRUE ~ "$$"
      ),
      price_numeric = case_when(
        price_level == "$" ~ 1,
        price_level == "$$" ~ 2,
        price_level == "$$$" ~ 3,
        price_level == "$$$$" ~ 4,
        price_level == "N/A" ~ 2,  # Unknown price defaults to medium
        TRUE ~ 2
      )
    )
  
  return(all_data)
}

# ============================================================================
# Get Default Meal Time Based on Current Time
# ============================================================================

get_default_meal_time <- function() {
  current_hour <- as.numeric(format(Sys.time(), "%H"))
  
  if (current_hour >= 7 && current_hour < 10) {
    return("Breakfast (7-10 AM)")
  } else if (current_hour >= 10 && current_hour < 14) {
    return("Lunch (12-2 PM)")
  } else if (current_hour >= 14 && current_hour < 21) {
    return("Dinner (6-9 PM)")
  } else if (current_hour >= 21 || current_hour < 7) {
    return("Late Night (9 PM+)")
  } else {
    return("Anytime")
  }
}

# ============================================================================
# Recommendation Algorithm
# ============================================================================

recommend_venues <- function(data, budget, address, meal_time, n_recommendations = 8, user_coords = NULL, search_text = "") {
  # Budget filtering
  budget_filter <- case_when(
    budget == "Low ($)" ~ 1,
    budget == "Medium ($$)" ~ 2,
    budget == "High ($$$)" ~ 3,
    budget == "Luxury ($$$$)" ~ 4,
    TRUE ~ 2
  )
  
  # Time period filtering
  time_categories <- if (meal_time == "Breakfast (7-10 AM)") {
    c("Cafe/Brunch", "Cafe")
  } else if (meal_time == "Lunch (12-2 PM)") {
    c("Cafe/Brunch", "Cafe", "Other", "Mexican")
  } else if (meal_time == "Dinner (6-9 PM)") {
    c("Other", "Bar", "Mexican")
  } else if (meal_time == "Late Night (9 PM+)") {
    c("Bar")
  } else {
    c("Cafe/Brunch", "Cafe", "Bar", "Other", "Mexican", "Milk Tea", "Juice")
  }
  
  # Filter data
  filtered <- data %>%
    filter(price_numeric <= budget_filter + 1) %>%
    filter(category %in% time_categories | grepl(paste(time_categories, collapse = "|"), category, ignore.case = TRUE))
  
  # Search filtering
  if (!is.null(search_text) && search_text != "") {
    filtered <- filtered %>%
      filter(grepl(search_text, name, ignore.case = TRUE) | 
             grepl(search_text, category, ignore.case = TRUE) |
             grepl(search_text, address, ignore.case = TRUE))
  }
  
  # Calculate distance if address or GPS coordinates are available
  if (!is.null(address) && address != "") {
    # Prioritize GPS coordinates, otherwise use Melbourne CBD center as reference
    if (!is.null(user_coords)) {
      center_lat <- user_coords$lat
      center_lon <- user_coords$lng
    } else {
      center_lat <- -37.8136
      center_lon <- 144.9631
    }
    
    filtered <- filtered %>%
      mutate(
        # Calculate straight-line distance and apply road coefficient 1.3
        distance = (distHaversine(
          cbind(lon, lat),
          c(center_lon, center_lat)
        ) / 1000) * 1.3  # Convert to kilometers and apply road coefficient
      )
  } else {
    filtered$distance <- 0
  }
  
  # Comprehensive scoring algorithm
  filtered <- filtered %>%
    mutate(
      # Rating weight (40%)
      rating_score = (rating / 5) * 0.4,
      # Price match score (30%)
      price_score = (1 - abs(price_numeric - budget_filter) / 4) * 0.3,
      # Distance weight (20%)
      distance_score = ifelse(distance > 0, (1 - pmin(distance / 5, 1)) * 0.2, 0.2),
      # Randomness (10%) - Increase recommendation diversity
      random_score = runif(n()) * 0.1,
      # Total score
      total_score = rating_score + price_score + distance_score + random_score
    ) %>%
    arrange(desc(total_score))
  
  # Return all matching recommendations (sorted by score)
  recommendations <- filtered %>%
    arrange(desc(total_score))
  
  return(recommendations)
}

# ============================================================================
# Route Planning and Distance Calculation
# ============================================================================

# Get route along streets using OSRM API
get_route_from_osrm <- function(from_lon, from_lat, to_lon, to_lat) {
  tryCatch({
    # OSRM public server API
    url <- sprintf(
      "http://router.project-osrm.org/route/v1/driving/%f,%f;%f,%f?overview=full&geometries=geojson",
      from_lon, from_lat, to_lon, to_lat
    )
    
    response <- jsonlite::fromJSON(url)
    
    if (response$code == "Ok" && length(response$routes) > 0) {
      route <- response$routes[1, ]
      return(list(
        distance = route$distance / 1000,  # Convert to kilometers
        duration = route$duration / 60,     # Convert to minutes
        geometry = route$geometry$coordinates[[1]]  # Route coordinates
      ))
    }
  }, error = function(e) {
    # If API fails, use estimated values
    return(NULL)
  })
  
  return(NULL)
}

calculate_route <- function(selected_venues) {
  if (nrow(selected_venues) < 2) {
    return(list(
      total_distance = 0,
      total_time = 0,
      route_details = data.frame(),
      route_geometries = list()
    ))
  }
  
  # Calculate distance for each segment
  route_details <- data.frame()
  route_geometries <- list()
  total_distance <- 0
  
  for (i in 1:(nrow(selected_venues) - 1)) {
    from <- selected_venues[i, ]
    to <- selected_venues[i + 1, ]
    
    # Try to get real route from OSRM
    osrm_route <- get_route_from_osrm(from$lon, from$lat, to$lon, to$lat)
    
    if (!is.null(osrm_route)) {
      # Use real distance and time returned by OSRM
      distance <- osrm_route$distance
      drive_time <- osrm_route$duration
      walk_time <- (distance / 5) * 60  # Walking speed 5km/h
      route_geometries[[i]] <- osrm_route$geometry
    } else {
      # If API fails, use estimated values
      straight_distance <- distHaversine(
        c(from$lon, from$lat),
        c(to$lon, to$lat)
      ) / 1000  # Kilometers
      
      # Apply road coefficient 1.3 to estimate actual road distance
      distance <- straight_distance * 1.3
      
      # Estimate time (walking speed 5km/h, driving speed 30km/h)
      walk_time <- (distance / 5) * 60  # Minutes
      drive_time <- (distance / 30) * 60  # Minutes
      route_geometries[[i]] <- NULL
    }
    
    route_details <- rbind(route_details, data.frame(
      from = from$name,
      to = to$name,
      distance_km = round(distance, 2),
      walk_time_min = round(walk_time, 1),
      drive_time_min = round(drive_time, 1)
    ))
    
    total_distance <- total_distance + distance
  }
  
  total_walk_time <- sum(route_details$walk_time_min)
  total_drive_time <- sum(route_details$drive_time_min)
  
  return(list(
    total_distance = round(total_distance, 2),
    total_walk_time = round(total_walk_time, 1),
    total_drive_time = round(total_drive_time, 1),
    route_geometries = route_geometries,
    route_details = route_details
  ))
}

# ============================================================================
# UI Interface
# ============================================================================

ui <- dashboardPage(
  skin = "black",
  
  # Header
  dashboardHeader(
    title = "Melbourne Dining",
    titleWidth = 300
  ),
  
  # Sidebar
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      menuItem("Main", tabName = "main", icon = icon("map-marked-alt")),
      menuItem("Analytics", tabName = "analytics", icon = icon("chart-bar"))
    ),
    
    hr(style = "border-color: rgba(0,0,0,0.1);"),
    h4("Filters", style = "padding-left: 15px; color: #2c3e50; font-weight: 300; font-size: 14px; letter-spacing: 1px;"),
    
    # Search box
    textInput(
      "search_text",
      "Search:",
      placeholder = "Enter restaurant name...",
      value = ""
    ),
    
    # Budget filter
    selectInput(
      "budget",
      "Budget:",
      choices = c(
        "Low ($)" = "Low ($)",
        "Medium ($$)" = "Medium ($$)",
        "High ($$$)" = "High ($$$)",
        "Luxury ($$$$)" = "Luxury ($$$$)"
      ),
      selected = "Medium ($$)"
    ),
    
    # Location selection
    div(
      style = "margin-bottom: 15px;",
      selectInput(
        "address",
        "Location:",
        choices = c(
          "Melbourne CBD" = "Melbourne CBD|-37.8136|144.9631",
          "Flinders Street Station" = "Flinders Street|-37.8183|144.9671",
          "Southern Cross Station" = "Southern Cross|-37.8184|144.9525",
          "Queen Victoria Market" = "QV Market|-37.8076|144.9568",
          "Federation Square" = "Fed Square|-37.8180|144.9691",
          "Docklands" = "Docklands|-37.8142|144.9386",
          "Carlton" = "Carlton|-37.8004|144.9672",
          "Fitzroy" = "Fitzroy|-37.7987|144.9789",
          "South Yarra" = "South Yarra|-37.8397|144.9931",
          "St Kilda" = "St Kilda|-37.8679|144.9810",
          "Use My GPS Location" = "GPS"
        ),
        selected = "Melbourne CBD|-37.8136|144.9631"
      )
    ),
    
    # Meal time
    selectInput(
      "meal_time",
      "Meal Time:",
      choices = c(
        "Breakfast (7-10 AM)" = "Breakfast (7-10 AM)",
        "Lunch (12-2 PM)" = "Lunch (12-2 PM)",
        "Dinner (6-9 PM)" = "Dinner (6-9 PM)",
        "Late Night (9 PM+)" = "Late Night (9 PM+)",
        "Anytime" = "Anytime"
      ),
      selected = get_default_meal_time()
    ),
    
    hr(style = "border-color: rgba(0,0,0,0.1);"),
    
    # Clear selection button
    div(
      style = "display: flex; justify-content: center; padding: 0 10px;",
      actionButton(
        "clear_selection",
        "CLEAR SELECTION",
        class = "clear_btn",
        style = "border-radius: 6px;"
      )
    ),
    
    hr(style = "border-color: rgba(0,0,0,0.1);"),
    
    # Tips
    div(
      style = "padding: 10px; font-size: 11px; color: #34495e;",
      icon("info-circle"),
      " Recommendations update automatically when you change filters."
    )
  ),
  
  # Main content
  dashboardBody(
    tags$head(
      # SortableJS CDN
      tags$script(src = "https://cdn.jsdelivr.net/npm/sortablejs@1.15.0/Sortable.min.js"),
      
      # JavaScript to get geolocation
      tags$script(HTML("
        Shiny.addCustomMessageHandler('getLocation', function(message) {
          if (navigator.geolocation) {
            navigator.geolocation.getCurrentPosition(
              function(position) {
                Shiny.setInputValue('user_coords', {
                  lat: position.coords.latitude,
                  lng: position.coords.longitude
                }, {priority: 'event'});
              },
              function(error) {
                alert('Unable to get location: ' + error.message);
              }
            );
          } else {
            alert('Geolocation is not supported by this browser.');
          }
        });
        
        // Initialize Sortable when venues list is rendered
        $(document).on('shiny:value', function(event) {
          if (event.name === 'selected_venues_ui') {
            setTimeout(function() {
              var el = document.getElementById('sortable-venues-container');
              if (el && !el.sortableInstance) {
                el.sortableInstance = Sortable.create(el, {
                  animation: 150,
                  ghostClass: 'sortable-ghost',
                  dragClass: 'sortable-drag',
                  handle: '.sortable-item',
                  onEnd: function(evt) {
                    var items = el.querySelectorAll('.sortable-item');
                    var order = [];
                    items.forEach(function(item) {
                      order.push(item.getAttribute('data-venue-name'));
                    });
                    Shiny.setInputValue('venue_order', order, {priority: 'event'});
                  }
                });
              }
            }, 100);
          }
        });
      ")),
      tags$style(HTML("
        /* Minimalist color scheme */
        .content-wrapper { 
          background-color: #fafafa; 
        }
        
        .main-header .logo {
          background-color: #ecf0f1 !important;
          color: #2c3e50 !important;
          font-weight: 300;
          letter-spacing: 1px;
        }
        
        .main-header .navbar {
          background-color: #ecf0f1 !important;
        }
        
        .main-header .navbar .sidebar-toggle {
          color: #2c3e50 !important;
        }
        
        .main-header .navbar .sidebar-toggle:hover {
          background-color: #d5dbdb !important;
        }
        
        .main-sidebar {
          background-color: #ecf0f1 !important;
        }
        
        .sidebar-menu > li > a {
          color: #2c3e50 !important;
          font-weight: 300;
        }
        
        .sidebar-menu > li > a > .fa {
          color: #34495e !important;
        }
        
        .sidebar-menu > li.active > a {
          background-color: #d5dbdb !important;
          border-left: 3px solid #3498db;
          color: #2c3e50 !important;
        }
        
        .sidebar-menu > li:hover > a {
          background-color: #d5dbdb !important;
        }
        
        /* Sidebar form controls */
        .sidebar label {
          color: #2c3e50 !important;
          font-weight: 300;
          font-size: 13px;
        }
        
        .sidebar .form-control {
          background-color: #ffffff;
          color: #2c3e50;
          border: 1px solid #bdc3c7;
        }
        
        .sidebar .form-control:focus {
          border-color: #3498db;
          background-color: #ffffff;
        }
        
        .sidebar hr {
          border-top: 1px solid rgba(0,0,0,0.1);
        }
        
        /* Minimalist box style */
        .box { 
          border-radius: 0;
          box-shadow: none;
          border: 1px solid #e0e0e0;
          background-color: #ffffff;
        }
        
        .box-header {
          border-bottom: 1px solid #e0e0e0;
          background-color: #ffffff;
        }
        
        .box-title {
          font-weight: 300;
          font-size: 16px;
          color: #2c3e50;
          letter-spacing: 0.5px;
        }
        
        /* Venue card minimalist style */
        .info-box { 
          cursor: pointer; 
          transition: all 0.3s ease;
          border-left: 3px solid transparent !important;
        }
        
        .info-box:hover { 
          transform: translateX(5px); 
          box-shadow: 0 2px 8px rgba(0,0,0,0.08);
          border-left-color: #3498db !important;
        }
        
        .leaflet-container { 
          border-radius: 0;
        }
        
        /* Minimalist scrollbar */
        ::-webkit-scrollbar {
          width: 6px;
        }
        ::-webkit-scrollbar-track {
          background: transparent;
        }
        ::-webkit-scrollbar-thumb {
          background: #bdc3c7;
          border-radius: 3px;
        }
        ::-webkit-scrollbar-thumb:hover {
          background: #95a5a6;
        }
        
        /* Minimalist cluster markers */
        .marker-cluster-small {
          background-color: rgba(52, 152, 219, 0.2);
        }
        .marker-cluster-small div {
          background-color: rgba(52, 152, 219, 0.6);
          color: white;
          font-weight: 300;
        }
        .marker-cluster-medium {
          background-color: rgba(241, 196, 15, 0.2);
        }
        .marker-cluster-medium div {
          background-color: rgba(241, 196, 15, 0.6);
          color: white;
          font-weight: 300;
        }
        .marker-cluster-large {
          background-color: rgba(231, 76, 60, 0.2);
        }
        .marker-cluster-large div {
          background-color: rgba(231, 76, 60, 0.6);
          color: white;
          font-weight: 300;
        }
        .marker-cluster {
          border-radius: 50%;
        }
        .marker-cluster div {
          border-radius: 50%;
          text-align: center;
          font-size: 12px;
          line-height: 30px;
        }
        
        /* Button minimalist style */
        .btn {
          border-radius: 0;
          font-weight: 300;
          letter-spacing: 0.5px;
          transition: all 0.3s ease;
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
      

     #clear_btn:hover {
        background-color: #eaeaea;
        border-color: #aaa;
      }
  
    
      #clear_btn span {
        letter-spacing: 0.3px;
      }
  
      .btn {
        width: 100%;
        text-align: center;
      }
  
      #clear_btn { margin-left: 4px; }
  
      .shiny-row {
        display: flex;
        gap: 8px;
      }
        .btn-warning {
          background-color: #f39c12;
          border-color: #f39c12;
          color: #ffffff !important;
        }
        
        .btn-warning:hover {
          background-color: #e67e22;
          border-color: #e67e22;
          color: #ffffff !important;
        }
        
        .btn-success {
          background-color: #27ae60;
          border-color: #27ae60;
          color: #ffffff !important;
        }
        
        .btn-success:hover {
          background-color: #229954;
          border-color: #229954;
          color: #ffffff !important;
        }
        
        /* Input minimalist style */
        .form-control {
          border-radius: 0;
          border: 1px solid #ddd;
          box-shadow: none;
        }
        
        .form-control:focus {
          border-color: #3498db;
          box-shadow: none;
        }
        
        /* Remove icons from titles */
        h4 strong {
          font-weight: 300;
        }
        
        /* DataTable styling */
        .dataTables_wrapper {
          font-family: -apple-system, BlinkMacSystemFont, sans-serif;
          color: #2c3e50;
        }
        
        table.dataTable thead th {
          background-color: #ecf0f1;
          color: #2c3e50;
          font-weight: 400;
          border-bottom: 2px solid #bdc3c7;
        }
        
        table.dataTable tbody td {
          color: #34495e;
        }
        
        table.dataTable tbody tr:hover {
          background-color: #f8f9fa !important;
        }
        
        /* Notification styling */
        .shiny-notification {
          border-radius: 0;
          border-left: 3px solid #3498db;
          font-weight: 300;
        }
        
        .shiny-notification-message {
          border-left-color: #3498db;
        }
        
        .shiny-notification-warning {
          border-left-color: #f39c12;
        }
        
        .shiny-notification-error {
          border-left-color: #e74c3c;
        }
        
        /* Sortable list styles */
        .sortable-list {
          list-style: none;
          padding: 0;
          margin: 0;
        }
        
        .sortable-item {
          transition: all 0.2s ease;
        }
        
        .sortable-item:hover {
          box-shadow: 0 2px 8px rgba(0,0,0,0.1);
        }
        
        .sortable-ghost {
          opacity: 0.4;
          background-color: #ecf0f1;
        }
        
        
        .sortable-drag {
          opacity: 0.8;
          box-shadow: 0 4px 12px rgba(0,0,0,0.15);
        }
        
        /* Optimize route button hover effect */
        #optimize_route:hover {
          transform: translateY(-1px);
          box-shadow: 0 4px 8px rgba(0,0,0,0.15) !important;
          background: linear-gradient(135deg, #764ba2 0%, #667eea 100%) !important;
        }
        
        #optimize_route:active {
          transform: translateY(0);
          box-shadow: 0 2px 4px rgba(0,0,0,0.1) !important;
        }
      "))
    ),
    
    tabItems(
      # Main page
      tabItem(
        tabName = "main",
        
        # Main content area
        fluidRow(
          # Selected venues list
          box(
            title = "SELECTED VENUES",
            status = "primary",
            solidHeader = FALSE,
            width = 5,
            height = 600,
            
            # Trip summary statistics
            uiOutput("trip_summary"),
            
            # Color legend and button row
            div(
              style = "display: flex; justify-content: space-between; align-items: center; padding: 8px 10px; margin-bottom: 10px; background-color: #f8f9fa; border-radius: 0; font-size: 11px; flex-wrap: wrap; gap: 10px;",
              # Color legend
              div(
                style = "display: flex; justify-content: flex-start; flex-wrap: wrap; flex: 1; min-width: 200px;",
                tags$span(
                  style = "display: flex; align-items: center; margin: 2px 5px;",
                  tags$span(style = "width: 20px; height: 3px; background-color: #3498db; margin-right: 5px;"),
                  tags$span("Restaurant", style = "color: #7f8c8d; font-weight: 300;")
                ),
                tags$span(
                  style = "display: flex; align-items: center; margin: 2px 5px;",
                  tags$span(style = "width: 20px; height: 3px; background-color: #9b59b6; margin-right: 5px;"),
                  tags$span("Bar", style = "color: #7f8c8d; font-weight: 300;")
                ),
                tags$span(
                  style = "display: flex; align-items: center; margin: 2px 5px;",
                  tags$span(style = "width: 20px; height: 3px; background-color: #f39c12; margin-right: 5px;"),
                  tags$span("Cafe", style = "color: #7f8c8d; font-weight: 300;")
                ),
                tags$span(
                  style = "display: flex; align-items: center; margin: 2px 5px;",
                  tags$span(style = "width: 20px; height: 3px; background-color: #e91e63; margin-right: 5px;"),
                  tags$span("Drinks", style = "color: #7f8c8d; font-weight: 300;")
                )
              ),
              # Optimize button
              actionButton(
                "optimize_route",
                "OPTIMIZE ROUTE",
                icon = icon("route"),
                style = "padding: 6px 16px; font-size: 12px; font-weight: 400; letter-spacing: 0.8px; border-radius: 3px; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); border: none; color: white; box-shadow: 0 2px 4px rgba(0,0,0,0.1); transition: all 0.3s ease; white-space: nowrap; flex-shrink: 0;"
              )
            ),
            
            div(
              style = "height: 400px; overflow-y: auto; overflow-x: hidden;",
              uiOutput("selected_venues_ui")
            )
          ),
          
          # Map
          box(
            title = "MAP",
            status = "primary",
            solidHeader = FALSE,
            width = 7,
            height = 600,
            
            leafletOutput("map", height = 530)
          )
        ),
        
        # Route details
        fluidRow(
          box(
            title = "ROUTE DETAILS",
            status = "primary",
            solidHeader = FALSE,
            width = 12,
            collapsible = TRUE,
            
            DTOutput("route_table")
          )
        )
      ),
      
      # Analytics page
      tabItem(
        tabName = "analytics",
        
        fluidRow(
          box(
            title = "RATING DISTRIBUTION",
            status = "primary",
            solidHeader = FALSE,
            width = 6,
            plotlyOutput("rating_plot", height = 300)
          ),
          
          box(
            title = "PRICE LEVEL DISTRIBUTION",
            status = "primary",
            solidHeader = FALSE,
            width = 6,
            plotlyOutput("price_plot", height = 300)
          )
        ),
        
        fluidRow(
          box(
            title = "CATEGORY DISTRIBUTION",
            status = "primary",
            solidHeader = FALSE,
            width = 6,
            plotlyOutput("category_plot", height = 300)
          ),
          
          box(
            title = "TOP RATED VENUES",
            status = "primary",
            solidHeader = FALSE,
            width = 6,
            plotlyOutput("top_venues_plot", height = 300)
          )
        )
      )
    )
  )
)

# ============================================================================
# Server Logic
# ============================================================================

server <- function(input, output, session) {
  
  # Load data
  all_venues <- reactiveVal(load_data())
  
  # Recommendation results
  recommendations <- reactiveVal(data.frame())
  
  # Selected venues
  selected_venues <- reactiveVal(data.frame())
  
  # User location coordinates
  user_location <- reactiveVal(NULL)
  
  # Listen to address selection changes
  observeEvent(input$address, {
    if (input$address == "GPS") {
      # Send JavaScript to get location
      session$sendCustomMessage(
        type = "getLocation",
        message = list()
      )
    } else {
      # Parse selected location coordinates
      parts <- strsplit(input$address, "\\|")[[1]]
      if (length(parts) == 3) {
        user_location(list(
          name = parts[1],
          lat = as.numeric(parts[2]),
          lng = as.numeric(parts[3])
        ))
      }
    }
  })
  
  # Receive GPS location from browser
  observeEvent(input$user_coords, {
    coords <- input$user_coords
    if (!is.null(coords)) {
      user_location(list(
        name = "My GPS Location",
        lat = coords$lat,
        lng = coords$lng
      ))
      showNotification(
        "GPS location detected successfully!",
        type = "message",
        duration = 3
      )
    }
  })
  
  # Auto recommendation - Listen to filter changes
  observe({
    req(all_venues())
    
    # Listen to all filter conditions
    input$budget
    input$address
    input$meal_time
    input$search_text
    user_location()
    
    # Auto update recommendations (show all matching results)
    recs <- recommend_venues(
      all_venues(),
      input$budget,
      input$address,
      input$meal_time,
      n_recommendations = 999,  # Set large value to show all results
      user_coords = user_location(),
      search_text = input$search_text
    )
    
    recommendations(recs)
  })
  
  # Clear selection
  observeEvent(input$clear_selection, {
    selected_venues(data.frame())
    showNotification("Selection cleared!", type = "warning", duration = 2)
  })
  
  # Trip summary statistics
  output$trip_summary <- renderUI({
    selected <- selected_venues()
    
    if (nrow(selected) == 0) {
      return(NULL)
    }
    
    # Calculate statistics
    total_venues <- nrow(selected)
    
    # Count by category
    restaurants <- sum(grepl("Restaurant|Other", selected$category, ignore.case = TRUE))
    bars <- sum(grepl("Bar", selected$category, ignore.case = TRUE))
    cafes <- sum(grepl("Cafe|Coffee|Brunch|Bakery", selected$category, ignore.case = TRUE))
    drinks <- sum(grepl("Milk Tea|Juice|Drink", selected$category, ignore.case = TRUE))
    
    # Calculate route if more than 1 venue
    total_distance <- 0
    total_walk_time <- 0
    
    if (nrow(selected) > 1) {
      route_info <- calculate_route(selected)
      if (!is.null(route_info$route_details)) {
        total_distance <- sum(route_info$route_details$distance_km, na.rm = TRUE)
        total_walk_time <- sum(route_info$route_details$walk_time_min, na.rm = TRUE)
      }
    }
    
    # Build summary UI
    div(
      style = "padding: 12px 10px; margin-bottom: 10px; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); border-radius: 3px; color: white;",
      div(
        style = "display: flex; justify-content: space-around; align-items: center; flex-wrap: wrap;",
        # Total venues
        div(
          style = "text-align: center; margin: 5px 10px;",
          div(style = "font-size: 24px; font-weight: 600;", total_venues),
          div(style = "font-size: 11px; font-weight: 300; opacity: 0.9;", "VENUES")
        ),
        # Distance
        if (total_distance > 0) {
          div(
            style = "text-align: center; margin: 5px 10px;",
            div(style = "font-size: 24px; font-weight: 600;", sprintf("%.1f", total_distance)),
            div(style = "font-size: 11px; font-weight: 300; opacity: 0.9;", "KM")
          )
        },
        # Walk time
        if (total_walk_time > 0) {
          div(
            style = "text-align: center; margin: 5px 10px;",
            div(style = "font-size: 24px; font-weight: 600;", sprintf("%.0f", total_walk_time)),
            div(style = "font-size: 11px; font-weight: 300; opacity: 0.9;", "MIN WALK")
          )
        }
      ),
      # Category breakdown
      if (total_venues > 0) {
        div(
          style = "margin-top: 8px; padding-top: 8px; border-top: 1px solid rgba(255,255,255,0.3); font-size: 11px; text-align: center;",
          tags$span(
            style = "margin: 0 8px;",
            if (restaurants > 0) sprintf("🍽️ %d Restaurant%s", restaurants, if(restaurants > 1) "s" else "")
          ),
          tags$span(
            style = "margin: 0 8px;",
            if (bars > 0) sprintf("🍷 %d Bar%s", bars, if(bars > 1) "s" else "")
          ),
          tags$span(
            style = "margin: 0 8px;",
            if (cafes > 0) sprintf("☕ %d Cafe%s", cafes, if(cafes > 1) "s" else "")
          ),
          tags$span(
            style = "margin: 0 8px;",
            if (drinks > 0) sprintf("🧋 %d Drink%s", drinks, if(drinks > 1) "s" else "")
          )
        )
      }
    )
  })
  
  # Selected venues list UI
  output$selected_venues_ui <- renderUI({
    selected <- selected_venues()
    
    if (nrow(selected) == 0) {
      return(
        div(
          style = "text-align: center; padding: 80px 20px;",
          h4("No venues selected", style = "color: #95a5a6; margin-top: 15px; font-weight: 300; font-size: 14px;"),
          p("Click 'Add to Trip' button on map markers to add venues", style = "color: #bdc3c7; font-size: 12px; font-weight: 300;")
        )
      )
    }
    
    venue_items <- lapply(1:nrow(selected), function(i) {
      venue <- selected[i, ]
      # Use name as unique identifier
      venue_id <- URLencode(venue$name, reserved = TRUE)
      
      # Determine border color based on category
      border_color <- if (grepl("Bar", venue$category, ignore.case = TRUE)) {
        "#9b59b6"  # Purple for bars
      } else if (grepl("Cafe|Coffee|Brunch|Bakery", venue$category, ignore.case = TRUE)) {
        "#f39c12"  # Orange for cafes
      } else if (grepl("Milk Tea|Juice|Drink", venue$category, ignore.case = TRUE)) {
        "#e91e63"  # Pink for drinks
      } else {
        "#3498db"  # Blue for restaurants
      }
      
      div(
        `data-venue-name` = venue$name,
        class = "info-box sortable-item",
        style = sprintf("margin-bottom: 12px; border-left: 5px solid %s; border-top: none; border-right: none; border-bottom: 1px solid #e0e0e0; padding: 12px 8px 12px 12px; background-color: #ffffff; position: relative; cursor: move; box-shadow: -2px 0 0 %s inset;", border_color, border_color),
        
        # Delete button
        actionButton(
          inputId = paste0("remove_", venue_id),
          label = icon("times"),
          style = "position: absolute; top: 8px; right: 8px; padding: 4px 8px; background-color: transparent; color: #95a5a6; border: none; cursor: pointer; font-size: 16px;",
          onclick = sprintf("Shiny.setInputValue('remove_venue', '%s', {priority: 'event'})", venue_id)
        ),
        
        # Color indicator badge
        tags$span(
          style = sprintf("position: absolute; top: 8px; left: 8px; width: 8px; height: 8px; border-radius: 50%%; background-color: %s;", border_color)
        ),
        
        # Title and rating
        h4(
          style = "margin: 0 30px 8px 0; padding-left: 12px; font-size: 15px; font-weight: 400; color: #2c3e50;",
          venue$name,
          tags$span(
            style = "display: block; color: #95a5a6; font-size: 12px; margin-top: 4px; font-weight: 300;",
            sprintf("★ %.1f", venue$rating)
          )
        ),
        
        # Category, price, distance
        p(
          style = "margin: 6px 0; padding-left: 12px; font-size: 12px; color: #7f8c8d; font-weight: 300;",
          tags$span(venue$category),
          tags$span(
            style = "margin-left: 15px; color: #2c3e50;",
            venue$price_level
          ),
          if (!is.null(venue$distance) && venue$distance > 0) {
            tags$span(style = "margin-left: 15px;", sprintf("%.1f km", venue$distance))
          }
        ),
        
        # Address (shortened)
        p(
          style = "font-size: 11px; color: #95a5a6; margin: 4px 0; padding-left: 12px; font-weight: 300;",
          substr(venue$address, 1, 40), 
          if (nchar(venue$address) > 40) "..." else ""
        ),
        
        # Description (shorter)
        if (!is.na(venue$description) && nchar(venue$description) > 0) {
          p(
            style = "font-size: 10px; color: #bdc3c7; font-style: italic; margin: 4px 0 0 0; padding-left: 12px; font-weight: 300;",
            substr(venue$description, 1, 60), 
            if (nchar(venue$description) > 60) "..." else ""
          )
        }
      )
    })
    
    # Return sortable container
    div(
      id = "sortable-venues-container",
      class = "sortable-list",
      venue_items
    )
  })
  
  # Listen to venue order changes from drag and drop
  observeEvent(input$venue_order, {
    if (!is.null(input$venue_order) && length(input$venue_order) > 0) {
      current_selected <- selected_venues()
      
      # Reorder selected venues based on drag order
      new_order <- input$venue_order
      reordered <- current_selected[match(new_order, current_selected$name), ]
      reordered <- reordered[!is.na(reordered$name), ]  # Remove NA rows
      
      if (nrow(reordered) > 0) {
        selected_venues(reordered)
      }
    }
  }, ignoreInit = TRUE)
  
  # Remove venue event
  observeEvent(input$remove_venue, {
    # Decode venue_id to get name
    remove_name <- URLdecode(input$remove_venue)
    current_selected <- selected_venues()
    
    # Remove from selection list
    current_selected <- current_selected %>%
      filter(name != remove_name)
    
    selected_venues(current_selected)
    
    showNotification(
      paste0("Removed: ", remove_name),
      type = "warning",
      duration = 2
    )
  })
  
  # "Add to Trip" button click event
  observeEvent(input$add_to_trip, {
    # Decode venue name
    venue_name <- URLdecode(input$add_to_trip)
    recs <- recommendations()
    clicked_venue <- recs %>% filter(name == venue_name)
    
    if (nrow(clicked_venue) > 0) {
      current_selected <- selected_venues()
      
      # Check if already selected
      if (!venue_name %in% current_selected$name) {
        # Add to selection list
        current_selected <- bind_rows(current_selected, clicked_venue)
        selected_venues(current_selected)
        
        showNotification(
          paste0("Added to trip: ", venue_name),
          type = "message",
          duration = 3
        )
      } else {
        showNotification(
          paste0("Already in trip: ", venue_name),
          type = "warning",
          duration = 2
        )
      }
    }
  })
  
  # Initial map rendering (only once)
  output$map <- renderLeaflet({
    loc <- user_location()
    
    # Base map - center on user location if available
    if (!is.null(loc)) {
      map <- leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = loc$lng, lat = loc$lat, zoom = 15)
    } else {
      map <- leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = 144.9631, lat = -37.8136, zoom = 14)
    }
    
    # Add user location marker
    if (!is.null(loc)) {
      map <- map %>%
        addAwesomeMarkers(
          lng = loc$lng,
          lat = loc$lat,
          icon = awesomeIcons(
            icon = "user",
            iconColor = "white",
            library = "fa",
            markerColor = "blue"
          ),
          popup = "<strong>Your Location</strong>",
          label = "You are here",
          layerId = "user_location"
        )
    }
    
    # Add layer control
    map <- map %>%
      addLayersControl(
        overlayGroups = c("recommendations", "selected"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
    map
  })
  
  # Update map markers when recommendations or selections change
  observe({
    recs <- recommendations()
    selected <- selected_venues()
    
    # Filter out already selected venues from recommendations
    if (nrow(selected) > 0 && nrow(recs) > 0) {
      recs <- recs %>% filter(!name %in% selected$name)
    }
    
    # Use leafletProxy to update markers without re-rendering the entire map
    leafletProxy("map") %>%
      clearGroup("recommendations") %>%
      clearGroup("selected") %>%
      clearShapes()  # Clear route lines
    
    # Add recommendation venue markers (with clustering)
    if (nrow(recs) > 0) {
      # Create popup content for each restaurant, including "Add to Trip" button
      recs$popup_content <- sapply(1:nrow(recs), function(i) {
        venue_name <- recs$name[i]
        venue_name_encoded <- URLencode(venue_name, reserved = TRUE)
        paste0(
          "<div style='min-width: 200px; font-family: -apple-system, BlinkMacSystemFont, sans-serif;'>",
          "<strong style='font-size: 14px; color: #2c3e50;'>", venue_name, "</strong><br/>",
          "<span style='color: #7f8c8d; font-size: 12px;'>", recs$category[i], "</span><br/>",
          "<span style='color: #34495e; font-size: 12px;'>Rating: ", recs$rating[i], "</span><br/>",
          "<span style='color: #34495e; font-size: 12px;'>Price: <strong style='color: #27ae60;'>", recs$price_level[i], "</strong></span><br/>",
          "<span style='color: #95a5a6; font-size: 11px;'>", substr(recs$address[i], 1, 50), ifelse(nchar(recs$address[i]) > 50, "...", ""), "</span><br/>",
          "<button onclick='Shiny.setInputValue(\"add_to_trip\", \"", venue_name_encoded, "\", {priority: \"event\"})' ",
          "style='margin-top: 10px; padding: 8px 15px; background-color: #3498db; color: white; border: none; border-radius: 0; cursor: pointer; font-weight: 300; width: 100%; letter-spacing: 0.5px;'>",
          "ADD TO TRIP",
          "</button>",
          "</div>"
        )
      })
      
      # Assign icons and colors based on category
      recs$icon_type <- sapply(recs$category, function(cat) {
        if (grepl("Bar", cat, ignore.case = TRUE)) {
          "wine-glass"
        } else if (grepl("Cafe|Coffee|Brunch|Bakery", cat, ignore.case = TRUE)) {
          "coffee"
        } else if (grepl("Milk Tea|Juice|Drink", cat, ignore.case = TRUE)) {
          "glass-martini"
        } else {
          "home"  # Restaurant - using home icon as fallback
        }
      })
      
      recs$marker_color <- sapply(recs$category, function(cat) {
        if (grepl("Bar", cat, ignore.case = TRUE)) {
          "purple"
        } else if (grepl("Cafe|Coffee|Brunch|Bakery", cat, ignore.case = TRUE)) {
          "orange"
        } else if (grepl("Milk Tea|Juice|Drink", cat, ignore.case = TRUE)) {
          "pink"
        } else {
          "blue"  # Restaurant
        }
      })
      
      # Create icons for each venue
      icons <- awesomeIcons(
        icon = recs$icon_type,
        iconColor = "white",
        library = "fa",
        markerColor = recs$marker_color
      )
      
      leafletProxy("map") %>%
        addAwesomeMarkers(
          data = recs,
          lng = ~lon,
          lat = ~lat,
          icon = icons,
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
    
    # Add selected venue markers (no clustering, always visible)
    if (nrow(selected) > 0) {
      # Assign icons for selected venues
      selected$icon_type <- sapply(selected$category, function(cat) {
        if (grepl("Bar", cat, ignore.case = TRUE)) {
          "wine-glass"
        } else if (grepl("Cafe|Coffee|Brunch|Bakery", cat, ignore.case = TRUE)) {
          "coffee"
        } else if (grepl("Milk Tea|Juice|Drink", cat, ignore.case = TRUE)) {
          "glass-martini"
        } else {
          "home"  # Restaurant - using home icon as fallback
        }
      })
      
      # Selected venues use red color
      selected_icons <- awesomeIcons(
        icon = selected$icon_type,
        iconColor = "white",
        library = "fa",
        markerColor = "red"
      )
      
      leafletProxy("map") %>%
        addAwesomeMarkers(
          data = selected,
          lng = ~lon,
          lat = ~lat,
          icon = selected_icons,
          popup = ~paste0(
            "<div style='font-family: -apple-system, BlinkMacSystemFont, sans-serif;'>",
            "<strong style='color: #e74c3c;'>SELECTED: ", name, "</strong><br/>",
            "<span style='color: #7f8c8d; font-size: 12px;'>", category, "</span><br/>",
            "<span style='color: #34495e; font-size: 12px;'>Rating: ", rating, "</span><br/>",
            "<span style='color: #34495e; font-size: 12px;'>Price: <strong style='color: #27ae60;'>", price_level, "</strong></span>",
            "</div>"
          ),
          label = ~paste("SELECTED:", name),
          group = "selected",
          layerId = ~name
        )
      
      # Add routes - use real street routes
      if (nrow(selected) > 1) {
        route_info <- calculate_route(selected)
        
        for (i in 1:(nrow(selected) - 1)) {
          # If OSRM returned route geometry data, use it
          if (!is.null(route_info$route_geometries[[i]])) {
            geometry <- route_info$route_geometries[[i]]
            leafletProxy("map") %>%
              addPolylines(
                lng = geometry[, 1],
                lat = geometry[, 2],
                color = "#e74c3c",
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
            # If no route data, use straight line (fallback)
            leafletProxy("map") %>%
              addPolylines(
                lng = c(selected$lon[i], selected$lon[i + 1]),
                lat = c(selected$lat[i], selected$lat[i + 1]),
                color = "#e74c3c",
                weight = 3,
                opacity = 0.7,
                dashArray = "5, 5",  # Dashed line indicates estimated route
                popup = "Estimated route (API unavailable)"
              )
          }
        }
      }
    }
  })
  
  # Optimize route button - reorder venues for shortest path
  observeEvent(input$optimize_route, {
    selected <- selected_venues()
    
    if (nrow(selected) < 2) {
      showNotification(
        "Please select at least 2 venues to optimize route!",
        type = "warning",
        duration = 3
      )
      return()
    }
    
    # Simple greedy algorithm for TSP (Traveling Salesman Problem)
    # Start from first venue, always go to nearest unvisited venue
    optimized <- selected[1, ]
    remaining <- selected[-1, ]
    
    while (nrow(remaining) > 0) {
      current <- optimized[nrow(optimized), ]
      
      # Calculate distances to all remaining venues
      distances <- sapply(1:nrow(remaining), function(i) {
        distHaversine(
          c(current$lon, current$lat),
          c(remaining$lon[i], remaining$lat[i])
        ) / 1000  # Convert to km
      })
      
      # Find nearest venue
      nearest_idx <- which.min(distances)
      optimized <- rbind(optimized, remaining[nearest_idx, ])
      remaining <- remaining[-nearest_idx, ]
    }
    
    selected_venues(optimized)
    
    showNotification(
      "Route optimized! Venues reordered for shortest path.",
      type = "message",
      duration = 3
    )
  })
  
  # Route details table
  output$route_table <- renderDT({
    selected <- selected_venues()
    
    if (nrow(selected) < 2) {
      return(data.frame(
        Message = "Select at least 2 venues and click 'Calculate Route' to see details"
      ))
    }
    
    route_info <- calculate_route(selected)
    
    datatable(
      route_info$route_details,
      options = list(
        pageLength = 10,
        dom = 't',
        ordering = FALSE
      ),
      rownames = FALSE,
      colnames = c(
        "From" = "from",
        "To" = "to",
        "Distance (km)" = "distance_km",
        "Walk Time (min)" = "walk_time_min",
        "Drive Time (min)" = "drive_time_min"
      )
    ) %>%
      formatStyle(
        columns = 1:5,
        backgroundColor = '#f9f9f9'
      )
  })
  
  # Analytics charts
  output$rating_plot <- renderPlotly({
    data <- all_venues()
    
    p <- ggplot(data, aes(x = rating)) +
      geom_histogram(bins = 20, fill = "#34495e", alpha = 0.8) +
      theme_minimal(base_family = "sans") +
      theme(
        text = element_text(color = "#2c3e50"),
        axis.text = element_text(color = "#7f8c8d"),
        panel.grid.major = element_line(color = "#ecf0f1"),
        panel.grid.minor = element_blank()
      ) +
      labs(x = "Rating", y = "Count", title = "")
    
    ggplotly(p) %>%
      layout(
        font = list(family = "sans-serif", color = "#2c3e50"),
        paper_bgcolor = "#ffffff",
        plot_bgcolor = "#fafafa"
      )
  })
  
  output$price_plot <- renderPlotly({
    data <- all_venues() %>%
      count(price_level) %>%
      mutate(price_level = factor(price_level, levels = c("$", "$$", "$$$", "$$$$")))
    
    p <- ggplot(data, aes(x = price_level, y = n, fill = price_level)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("#95a5a6", "#7f8c8d", "#34495e", "#2c3e50")) +
      theme_minimal(base_family = "sans") +
      theme(
        text = element_text(color = "#2c3e50"),
        axis.text = element_text(color = "#7f8c8d"),
        panel.grid.major = element_line(color = "#ecf0f1"),
        panel.grid.minor = element_blank(),
        legend.position = "none"
      ) +
      labs(x = "Price Level", y = "Count", title = "")
    
    ggplotly(p) %>%
      layout(
        font = list(family = "sans-serif", color = "#2c3e50"),
        paper_bgcolor = "#ffffff",
        plot_bgcolor = "#fafafa"
      )
  })
  
  output$category_plot <- renderPlotly({
    data <- all_venues() %>%
      count(category) %>%
      arrange(desc(n)) %>%
      head(10)
    
    p <- ggplot(data, aes(x = reorder(category, n), y = n)) +
      geom_bar(stat = "identity", fill = "#34495e", alpha = 0.8) +
      coord_flip() +
      theme_minimal(base_family = "sans") +
      theme(
        text = element_text(color = "#2c3e50"),
        axis.text = element_text(color = "#7f8c8d"),
        panel.grid.major = element_line(color = "#ecf0f1"),
        panel.grid.minor = element_blank(),
        legend.position = "none"
      ) +
      labs(x = "", y = "Count", title = "")
    
    ggplotly(p) %>%
      layout(
        font = list(family = "sans-serif", color = "#2c3e50"),
        paper_bgcolor = "#ffffff",
        plot_bgcolor = "#fafafa"
      )
  })
  
  output$top_venues_plot <- renderPlotly({
    data <- all_venues() %>%
      arrange(desc(rating)) %>%
      head(10)
    
    p <- ggplot(data, aes(x = reorder(name, rating), y = rating, fill = rating)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_gradient(low = "#95a5a6", high = "#2c3e50") +
      theme_minimal(base_family = "sans") +
      theme(
        text = element_text(color = "#2c3e50"),
        axis.text = element_text(color = "#7f8c8d"),
        panel.grid.major = element_line(color = "#ecf0f1"),
        panel.grid.minor = element_blank(),
        legend.position = "none"
      ) +
      labs(x = "", y = "Rating", title = "")
    
    ggplotly(p) %>%
      layout(
        font = list(family = "sans-serif", color = "#2c3e50"),
        paper_bgcolor = "#ffffff",
        plot_bgcolor = "#fafafa"
      )
  })
}

# ============================================================================
# Run Application
# ============================================================================

shinyApp(ui = ui, server = server)
