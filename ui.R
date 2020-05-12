library('shiny')
library('shinyjs')
library('shinythemes')
library('shinydashboard')
library('shinycssloaders')
library('dashboardthemes')

# UI
# 1) Allow user to selected up to two strategies
# 2) Create control parameter settings for these two strategies [or just one]
# 3) Create checkboxes so that the user can see multiple strategies [gray those for which a combination does not exist]

sidebar <- dashboardSidebar(
  width = 350,
  sidebarMenu(
    menuItem('Introduction', tabName = 'introduction', icon = icon('file-alt')),
    menuItem('Exit Strategies', tabName = 'exit', icon = icon('dashboard')),
    menuItem('Model Details', tabName = 'details', icon = icon('info-circle')),
    menuItem('About', tabName = 'about', icon = icon('address-card'))
  )
)

body <- dashboardBody(
  shinyjs::useShinyjs(),
  
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
            effects of different exit strategies.<p>"
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
            
            checkboxGroupInput(
              'exit',
              'Type of Exit Strategy',
              inline = FALSE,
              choices = c(
                'Radical Opening' = 'radical-opening',
                'Phased Opening' = 'phased-opening',
                'Flattening the Curve' = 'flattening-curve',
                'Contact Tracing' = 'contact-tracing',
                'Intermittent Lockdown' = 'intermittent-lockdown'
              )
            )
          )
        ),
        
        column(
          width = 9,
          box(
            collapsible = FALSE,
            title = textOutput('visualisation_name'),
            width = NULL, solidHeader = TRUE, status = 'primary',
            plotOutput('exit_visualisation', height = '600px') %>% withSpinner(color = '#0dc5c1')
          )
        ),

        column(
          width = 3,
          tabBox(
            width = NULL,
            id = 'tabset', title = '',
            
            tabPanel(
              title = 'Key Parameters',
              width = NULL,
              status = 'warning',
              collapsible = FALSE, solidHeader = TRUE,
              uiOutput('exit_parameters'),
              actionButton('run', 'Run')
            ),
            
            tabPanel(
              title = 'Sensitivity',
              withMathJax(),
              
              width = NULL,
              status = 'warning',
              collapsible = FALSE, solidHeader = TRUE,
              selectInput(
                'R0', withMathJax('\\( R_0 \\)'),
                choices = c('2' = 2, '2.5' = 2.5, '3' = 3), selected = 2.5
              ),
              selectInput(
                'infectiousness', 'Average Duration of Infectiousness',
                choices = c('8 Days' = 8, '10 Days' = 10, '12 Days' = 12), selected = 10
              ),
              selectInput(
                'incubation', 'Average Incubation Time',
                choices = c('4.5 Days' = 4.5, '5.5 Days' = 5.5, '6.5 Days' = 6.5), selected = 5.5
              ),
              actionButton('run', 'Run')
            )
          )
        )
      )
    ),
  
    tabItem(
      tabName = 'details'
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