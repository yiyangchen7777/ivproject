library('shiny')
library('ggplot2')
library('ggiraph')
library('shinyjs')

# Load the GEOM90007 Tableau in Shiny library
source('tableau-in-shiny-v1.2.R')

births_data <- read.csv('Births_summary_with_id.csv')

##################
# USER INTERFACE #
##################

births_tab <- tabPanel(
  title='Births',
  h2('Births in Australia'),
  splitLayout(
    girafeOutput('plot_births'),
    tableauPublicViz(
      id='tableauViz',       
      url='https://public.tableau.com/views/SampleTableauembedforShinyintegrationlab/Hospitalstreemap',
      height="300px"
    ),
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
}

#############
# Run Shiny #
#############

shinyApp(ui, server, options=list(launch.browser=TRUE))
