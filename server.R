library('shiny')
source('helpers.R')

IC_adm_data <- fread('data/IC_NL.csv')
scen_output <- as.data.table(readRDS(file = 'data/scen_output.rds'))
scen_description <- as.data.table(readRDS(file = 'data/scen_description.rds'))


shinyServer(function(input, output, session) {
  
  strategies <- list(
    'radical-opening' = list(
      name = 'Radical Opening'
    ),
    'flattening-curve' = list(
      name = 'Flattening the Curve'
    ),
    'phased-opening' = list(
      name = 'Phased Opening'
    ),
    'contact-tracing' = list(
      name = 'Contact Tracing'
    ),
    'intermittent-lockdown' = list(
      name = 'Intermittent Lockdown'
    )
  )
  
  exit_strategy <- reactiveValues(data = NULL)
  previous_exit_strategies <- reactiveValues(data = NULL)
  
  
  observe({
    choice <- input$exit
    choices <- c(
      'Radical Opening' = 'radical-opening',
      'Phased Opening' = 'phased-opening',
      'Flattening the Curve' = 'flattening-curve',
      'Contact Tracing' = 'contact-tracing',
      'Intermittent Lockdown' = 'intermittent-lockdown'
    )
    
    nr_exit <- length(choice)
    exit_strategy$data <- input$exit
    
    # User wants to click more than 2 boxes --- do not allow!
    if (!is.null(choice)) {
      
      if (nr_exit > 2) {
        updateCheckboxGroupInput(
          session, 'exit', 'Type of Exit Strategy',
          choices = choices, selected = previous_exit_strategies$names
        )
      } else {
        previous_exit_strategies$names <- exit_strategy$data
      }
    }
    
    # User only choose one exit strategy
    if (nr_exit == 1) {
      exit_strategy$names <- strategies[[choice]][['name']]
      exit_strategy$title <- strategies[[choice]][['name']]
      
    } else if (nr_exit == 2) {
      
      exit_strategy$names <- c(
        strategies[[choice[1]]][['name']],
        strategies[[choice[2]]][['name']]
      )
      
      exit_strategy$title <- paste0(
        exit_strategy$names[1], ' vs. ', exit_strategy$names[2]
      )
      
    } else {
      exit_strategy$title <- ''
    }
  })
  
  # Give the exit strategy box a name
  output$visualisation_name <- renderText({
    exit_strategy$title
  })
  
  # Create the exit parameter UI
  output$exit_parameters <- renderUI({
    exit <- exit_strategy$data
    
    if (!is.null(exit)) {
      create_exit_parameters(exit, exit_strategy$names)
    }
  })
  
  visualize <- eventReactive(input$run, {
    strategies <- exit_strategy$data
    
    if (!is.null(strategies)) {
      visualize_exit_strategy(
        strategies, input, scen_output, scen_description, IC_adm_data
      )
    }
  })
  
  output$exit_visualisation <- renderPlot({
    visualize()
  })
})
