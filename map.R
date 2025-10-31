# ---------------- detail_ui.R ----------------
detail_ui <- function(id, all) {
  ns <- NS(id)
  
  tabPanel(
    "Detail",
    fluidRow(
      column(
        12,
        selectizeInput(
          ns("pick"), label = NULL,
          choices = c('', sort(unique(stats::na.omit(all$name)))),
          selected = '',
          options  = list(placeholder = "search keyword…", create = FALSE),
          width    = "100%"
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        div(class = "hero-card",
            uiOutput(ns("shopImage")),
            br(),
            div(class = "meta-card", htmlOutput(ns("linkPhoneAddress")))
        ),
        br(),
        div(class = "kpi-card",
            uiOutput(ns("ratingProgress")),
            br(),
            uiOutput(ns("kpisText"))
        ),
        br(),
        div(class = "desc-card", uiOutput(ns("shopDesc")))
      ),
      
      column(
        width = 6,
        div(class = "hero-card",
            h4("Where are its peers nearby?"),
            div(id = ns("peerMapWrapper"), leafletOutput(ns("peerMap"), height = 360)),
            tags$small(class = "muted", id = ns("peerCaption"))
        ),
        br(),
        div(class = "hero-card",
            h4("Opening hours (local)"),
            uiOutput(ns("openNowRight")),
            plotOutput(ns("openHoursPlot"), height = 220)
        )
      )
    )
  )
}

