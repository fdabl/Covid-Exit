library('shiny')
library('shinythemes')
library('shinydashboard')
library('shinycssloaders')
library('dashboardthemes')


sidebar <- dashboardSidebar(
  width = 350,
  sidebarMenu(
    menuItem('Introduction', tabName = 'introduction', icon = icon('file-alt')),
    menuItem('Exit Strategies', tabName = 'exit', icon = icon('dashboard')),
    menuItem('About', tabName = 'about', icon = icon('address-card'))
  )
)

body <- dashboardBody(
  shinyDashboardThemes(
    theme = 'grey_light'
  ),
  
  tabItems(
    tabItem(
      tabName = 'introduction',
      fluidPage(
        fluidRow(
          box(
            width = 1000,
            tags$h2(
              'What are the effects of different exit strategies?', align = 'center'
            ),
            tags$br(),
            HTML(
            "<p style = 'font-size: 120%; text-align: center;'>This app uses the model described in de Vlas & Coffeng
            (<a href='https://www.medrxiv.org/content/10.1101/2020.03.29.20046011v2' target='_blank'>2020</a>) to explore the
            effects of various different exit strategies.<p>"
            )
          )
        )
      )
    ),
    
    tabItem(
      tabName = 'exit',
      fluidRow(
        
        column(
          width = 12,
          box(
            collapsible = TRUE,
            title = 'Select an Exit Strategy', width = NULL, solidHeader = TRUE, status = 'primary',
            selectInput(
              'exit',
              'Type of Exit Strategy',
              choices = c(
                'Radical Opening' = 'radical-opening',
                'Phased Opening' = 'phased-opening',
                'Flattening the Curve' = 'flatten-curve',
                'Contact Tracing' = 'contact-tracing'
              )
            )
          )
        ),
        
        
        column(
          width = 8,
          box(
            collapsible = FALSE,
            title = textOutput('visualisation_name'),
            width = NULL, solidHeader = TRUE, status = 'primary',
            uiOutput('exit_visualisation')
          )
        ),

        column(
          width = 4,
          box(
            title = 'Parameters',
            width = NULL,
            status = 'warning',
            collapsible = FALSE, solidHeader = TRUE,
            uiOutput('exit_parameters'),
            actionButton('run', 'Run')
          )
        )
      )
    ),
    
    tabItem(
      tabName = 'about',
      fluidPage(
        box(
          width = 1000,
          h3('About', align = 'center'),
          HTML(
            "<p style = 'font-size: 120%; text-align: center;'>
            This web interface was developed by <a href='https://twitter.com/fdabl' target='_blank'>Fabian Dablander</a>
            together with <a href='https://twitter.com/luc_coffeng' target='_blank'>Luc Coffeng</a> as a
            <a href='http://scienceversuscorona.com/' target='_blank'>Science versus Corona</a> project
            <p>"
            )
        )
      )
    )
  )
)


dashboardPage(
  dashboardHeader(title = 'COVID-19 Exit Strategies', titleWidth = 350),
  sidebar,
  body
)