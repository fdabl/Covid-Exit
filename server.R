library('shiny')
library('shinyjs')
source('helpers.R')

# Load in the relevant data
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
    
    # User only chose one exit strategy. Need to unblock all elements
    if (nr_exit == 1) {
      
      shinyjs::enable(selector = '#exit')
      exit_strategy$names <- strategies[[choice]][['name']]
      exit_strategy$title <- strategies[[choice]][['name']]
      
    # User chose two exit strategies. Need to block all other elements
    } else if (nr_exit == 2) {
      
      # Block other elements from being clicked on
      others <- choices[!sapply(choices, function(ch) ch %in% choice)]
      blocked_elements <- paste0("#exit input[value='", others, "']")
      shinyjs::disable(selector = blocked_elements)
      
      # TODO: Change colour of other elements
      # shinyjs::runjs("document.getElementsByName('#exit').style.color = 'grey'")
      
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
    
    if (!is.null(exit) && length(exit) <= 2) {
      create_exit_parameters(exit, exit_strategy$names)
    }
  })
  
  # Visualize the exit strategies
  visualize <- eventReactive(input$run, {
    strategies <- exit_strategy$data
    
    if (!is.null(strategies) && length(strategies) <= 2) {
      visualize_exit_strategy(
        strategies, input, scen_output, scen_description, IC_adm_data
      )
    }
  })
  
  output$exit_visualisation <- renderPlot({
    visualize()
  })
})
