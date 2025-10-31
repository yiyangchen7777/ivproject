library('shiny')
library('ggplot2')
library('ggiraph')
library('shinyjs')
library('tidyr')
library('dplyr')

# Load the GEOM90007 Tableau in Shiny library
source('tableau-in-shiny-v1.2.R')

births_data <- read.csv('Births_summary_with_id.csv')

hosp_data <- read.csv('Hospitals in Australia with childbirth stats.csv')

##################
# USER INTERFACE #
##################

births_tab <- tabPanel(
  title='Births',
  h2('Births in Australia'),
  verticalLayout(
    splitLayout(
      girafeOutput('plot_births'),
      tableauPublicViz(
        id='tableauViz',       
        url='https://public.tableau.com/views/SampleTableauembedforShinyintegrationlab/Hospitalstreemap',
        height="300px"
      ),
    ),
    girafeOutput('plot_hosp_births_timeline')
  )
)

ui <- navbarPage(
  header=setUpTableauInShiny(),
  title='Population growth in Australia',
  births_tab
)

################
# SHINY SERVER #
################

server <- function(input, output, session) {
  output$plot_births <- renderGirafe({
    p <- ggplot(births_data) +
      aes(x=Region, y=X2020, data_id=Region) +
      geom_bar_interactive(stat='identity', width=0.8, fill='#8f00b6') +
      scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
      labs(x='State', y='Births') +
      theme(panel.background=element_blank(),
            panel.grid.major.y=element_line(color='#e2e2e2'),
            axis.ticks=element_blank()) +
      ggtitle("Births by state in 2020")
    
    girafe(ggobj=p, height_svg=3)
  })
  
  # React to clicks on the bar chart
  # (See Lab 7 (page 7.6.2) for an explanation of this code)
  observeEvent(input$plot_births_selected, {
    # Clear selection from bar chart
    session$sendCustomMessage(type='plot_births_set', message=character(0))
    
    # Filter Tableau viz by the state that was clicked on the bar chart
    state <- input$plot_births_selected
    runjs(sprintf('let viz = document.getElementById("tableauViz");
        let sheet = viz.workbook.activeSheet;
        sheet.applyFilterAsync("State", ["%s"], FilterUpdateType.Replace);', state))
  })
  
  output$plot_hosp_births_timeline <- renderGirafe({
    # Find the name of the hospital clicked by the user
    hosp_name <- input$tableauViz_mark_selection_changed$Name[1]
    
    # If no hospital selected, stop
    if(is.null(hosp_name)) return()
    
    # Filter the births data to the hospital clicked by the user, and
    # transform it into the format required by ggplot's geom_line
    hosp_births_timeline_data <- hosp_data %>% 
      filter(Name == hosp_name) %>% 
      gather(key=Year, value=Births, Childbirths.2012:Childbirths.2021)
    
    p <- ggplot(hosp_births_timeline_data) +
      aes(x=Year, y=Births, group=Name, tooltip='Whatevs, Kevs') +
      geom_line_interactive() +
      scale_x_discrete(labels = 2012:2021) +
      scale_y_continuous(limits = c(0, max(hosp_births_timeline_data$Births))) +
      theme(panel.background=element_blank(),
            panel.grid.major.y=element_line(color='#e2e2e2'),
            axis.ticks=element_blank()) +
      ggtitle(paste("Childbirth trend at", hosp_name))
    
    girafe(ggobj=p, height_svg=4)
  })
}

#############
# Run Shiny #
#############

shinyApp(ui, server, options=list(launch.browser=TRUE))
