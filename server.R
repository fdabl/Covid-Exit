library('shiny')
source('helpers.R')

IC_adm_data <- fread('data/IC_NL.csv')
scen_output <- as.data.table(readRDS(file = 'data/scen_output.rds'))
scen_description <- as.data.table(readRDS(file = 'data/scen_description.rds'))


show_figure <- function(session, path, alt = '') {
  # Get size of window from the session
  width <- session$clientData$output_no_intervention_plot_width
  height <- session$clientData$output_no_intervention_plot_height
  
  pixelratio <- session$clientData$pixelratio # what is this?
  
  x <- readImage(path)
  # y <- resize(x, w = width, h = height)
  y <- resize(x, h = height)
  y
  
  # # Return a list containing the filename
  # list(
  #   src = path,
  #   contentType = 'image/png',
  #   width = width,
  #   height = height,
  #   alt = alt
  # )
}


shinyServer(function(input, output, session) {
  
  strategies <- list(
    'radical-opening' = list(
      name = 'Radical Opening'
    ),
    'flatten-curve' = list(
      name = 'Flattening the Curve'
    ),
    'phased-opening' = list(
      name = 'Phased Opening'
    ),
    'contact-tracing' = list(
      name = 'Contact Tracing'
    )
  )
  
  exit_strategy <- reactiveValues(data = NULL)
  
  observeEvent(input$exit, {
    exit_strategy$data <- input$exit
    exit_strategy$name <- strategies[[input$exit]][['name']]
  })
  
  output$visualisation_name <- renderText({
    exit_strategy$name
  })
  
  output$exit_visualisation <- renderUI({
    res <- NULL
    name <- exit_strategy$name
    strategy <- exit_strategy$data
    
    if (strategy == 'radical-opening') {
      res <- plotOutput('radical_opening')
    }
    
    res
  })
  
  output$exit_parameters <- renderUI({
    res <- tagList()
    strategy <- exit_strategy$data
    
    if (strategy == 'radical-opening') {
      res <- tagList()
      
      res[[1]] <-selectInput(
        'radical_transmission', 'Transmission',
        choices = c('25%' = 0.25, '50%' = 0.50, '75%' = 0.75)
      )
      
      res[[2]] <- sliderInput('slider', 'Slider', 0, 1, 1, .01)
    }
    
    res
  })
  
  
  output$radical_opening <- renderPlot({
    p <- plot_scen(
      x = scen_output[par_set == 1],
      y = scen_description[par_set == 1],
      IC_adm_data = IC_adm_data
    )
    
    p
  })
  
  output$flatten_curve <- renderPlot({
    hist(runif(100))
  })
})
