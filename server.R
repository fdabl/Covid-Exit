library('shiny')
library('shinyjs')
source('helpers.R')


# Mean of Monte carlo would be nice!
shinyServer(function(input, output, session) {
  
  strategies <- list(
    'radical-opening' = list(
      name = 'Radical Opening'
    ),
    'flattening-curve' = list(
      name = 'Flattening the Curve'
    ),
    'phased-opening' = list(
      name = 'Phased Lift of Control'
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
      'Phased Lift of Control' = 'phased-opening',
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
  
  # Explanation plots
  output$explanation_radical_opening <- renderPlot({
    visualize_exit_strategy('radical-opening', NULL, scen_output, scen_description, IC_adm_data, legend = 'inside')
  })
  
  output$explanation_flattening_curve <- renderPlot({
    inp <- list('flattening-curve' = c('scenario-1', 'scenario-2', 'scenario-3'))
    visualize_exit_strategy('flattening-curve', inp, scen_output, scen_description, IC_adm_data)
  })
  
  output$explanation_phased_opening <- renderPlot({
    inp <- list('phased-opening' = c('scenario-1', 'scenario-3'))
    visualize_exit_strategy('phased-opening', inp, scen_output, scen_description, IC_adm_data)
  })
  
  output$explanation_contact_tracing <- renderPlot({
    inp <- list(
      'contact_tracing_contact_reduction' = c('60%', '70%'),
      'contact_tracing_lockdown' = '0 days',
      'contact_tracing_trace_probability' = '60%',
      'contact_tracing_tracing_delay' = '2 days'
      
    )
    visualize_exit_strategy('contact-tracing', inp, scen_output, scen_description, IC_adm_data)
  })
  
  output$explanation_intermittent_lockdown <- renderPlot({
    inp <-list('intermittent-lockdown' = c('scenario-1', 'scenario-2', 'scenario-3'))
    visualize_exit_strategy('intermittent-lockdown', inp, scen_output, scen_description, IC_adm_data)
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
