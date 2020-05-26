library('shiny')
library('shinyjs')
library('shinythemes')
library('shinydashboard')
library('shinycssloaders')
library('dashboardthemes')


sidebar <- dashboardSidebar(
  width = 350,
  sidebarMenu(
    menuItem('Introduction', tabName = 'introduction', icon = icon('file-alt')),
    menuItem('Interactive Exploration', tabName = 'exit', icon = icon('dashboard')),
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
            width = 12,
            tags$h2(
              'What are the effects of different exit strategies?', align = 'center'
            ),
            tags$br(),
            HTML(
            "<p style = 'font-size: 120%; text-align: center;'>This app uses the model described in de Vlas & Coffeng
            (<a href='https://www.medrxiv.org/content/10.1101/2020.03.29.20046011v2' target='_blank'>2020</a>) to explore the
            effects of different exit strategies. You can find a brief description of the exit strategies below the model explanation.
            Under the tab <i>Interactive Exploration</i> on the left, you can further explore these exit strategies.</p>"
            )
          )
        ),
        
        fluidRow(
          box(
            status = 'primary',
            solidHeader = TRUE,
            collapsible = TRUE,
            title = 'Model Explanation',
            width = 12,
            HTML(
              "
              <p>The model follows the SEIR structure and is described in De Vlas & Coffeng (2020, Suppl. 1).
              Here, we briefly summarize the main points.</p>
              
              <p>First, the model assumes that the population is composed of a susceptible, exposed (i.e., incubating),
              infected, and removed subpopulations. Crucially, it assumes that reinfections do not occur, that is, that immunity
              lasts for life (or at least until the end of the simulation).</p>
              
              <p>Second, individuals are assumed to live in clusters (e.g., villages, neighborhoods), which are part of superclusters
              (e.g. larger urban areas, provinces), which together make up a region or country. The model allows for
              heterogeneity among contact rates of individuals and a preferential mixing of individuals with similar
              contact behaviour (i.e., assortative mixing), leading to some clusters having higher or lower average contact rates
              than the population as a whole on average. Interventions can be simulated at both the level of the whole population and at the
              level of superclusters, where the geographical boundaries of implementation units for interventions do not necessarily have to
              match the geographical boundaries of transmission units.</p>
              
              
              <p>The model allows simulation of two broad categories of interventions: (1) interventions that reduce transmission for all
              individuals and (2) contact tracing leading to isolation of infectious cases and quarantining of exposed contacts.
              These two modalities can be simulated at the same time and can change in intensity and duration throughout the simulation.
              The first modality, overall reductions in transmission, is simulated at the level of implementation units (superclusters),
              and allows for partial uptake of interventions as well as inter-individual variation in participation / adherence to interventions
              in case of uptake. The second modality, contact tracing, involves two processes. The first is that infectious / symptomatic individuals
              are at a continuous (user-defined) “risk” of being identified and isolated. The second is the (user-defined) probability that an exposed
              individual is detected and quarantined before they become infectious. Isolated and quarantined individuals contribute to and are exposed
              to transmission to a (user-defined) lesser degree than the rest of the population. As the model does not explicitly simulate transmission
              chains (i.e., who infects whom), exposed and infectious cases are identified in a random and independent fashion.</p>
              "
            )
          )
        ),
        
        fluidRow(
          column(
            width = 12,
            box(
              width = 6,
              status = 'primary',
              solidHeader = TRUE,
              collapsible = TRUE,
              title = 'Radical Opening',
              HTML(
                "
                <div style='float: right;'>
                  <img src='Radical-Opening.png' width=375 height=500>
                </div>
                
                <p>The Radical Opening exit strategy, as suggested in the media (e.g., by Klaas Hummel and Grandjean and Ozdemir), involves a sudden lift
                of the lockdown to return to business-as-usual. The main benefit of this strategy is that it would swiftly lead to herd immunity,
                after which the epidemic would die off within less than 100 days. Unfortunately, the costs would be vast.</p>
                
                <p>The Figure on the right shows that the number of infections would increase dramatically, leading to a substantial overburdening of the
                healthcare system (red dashed line). Ethically, this is unacceptable.</p>
                
                <p>However, this strategy also has economic consequences that are overlooked by its proponents. Things would not go back to normal
                immediately. In fact, Pichler et al. (2020) estimate that opening all industries would still lead to a 16%
                reduction of GDP compared to pre-lockdown levels for the UK case. A less radical opening strategy would lead to a
                roughly 17% reduction; paying with so many lives for so little economic gain is foolhardy.</p>
                
                <p>In contrast to other exit strategies discussed here, the radical opening exit strategy does not have additional parameters.</p>
                "
              )
            ),
            
            box(
              width = 6,
              status = 'primary',
              solidHeader = TRUE,
              collapsible = TRUE,
              title = 'Phased Opening',
              HTML(
                "
                <div style='float: right;'>
                  <img src='Phased-Opening.png' width=375 height=500>
                </div>
                
                <p>This exit strategy lifts the lockdown in different areas at different times. It is described in more detail in
                De Vlas & Coffeng (2020). Given a certain IC capacity the strategy can be adapted in terms of (a) the number of phases,
                (b) the time intervals between phases, and (c) the level of control measures in areas where they are still in force.
                Applied to the Netherlands, the idea is to split the country into twenty areas in which the lockdowns are subsequently lifted.</p>
                
                <p>The Figure on the right shows that this strategy can be designed such that it does not overburden the healthcare system,
                resulting in vastly fewer deaths than the radical opening strategy. However, the design on the right (20 phases and a maximum
                IC capacity of 108 beds per 1 million) requires roughly 750 days  until all measures can be lifted, and achieves a level
                of 80% immunity. As you can explore in the interactive part of the app, this strategy comes in three variants.</p>
                "
              )
            )
          )
        )
      ),
      
      fluidRow(
        column(
          width = 12,
          box(
            width = 6,
            status = 'primary',
            solidHeader = TRUE,
            collapsible = TRUE,
            title = 'Flattening the Curve',
            HTML(
              "
                
              <div style='float: right;'>
                <img src='Flattening-Curve.png' width=375 height=500>
              </div>
              
              <p>The concept of “flattening the curve” has become widely known. This exit strategy proposes to balance the number
              of infections such that the healthcare system does not become overburdened. A key parameter here is how strongly
              transmission is reduced by interventions.</p>
                
              <p>The Figure on the right shows three different scenarios in which transmissions are reduced to 30%, 35%, or
              40% in the first period. This illustrates the delicate balance required to make this exit strategy work: in the
              first period after lockdown, interventions would have to be relaxed such that transmission resumes at 35% of its potential level.
              If interventions are relaxed too quickly too early (e.g., transmission at 40% of its potential), the number of cases will exceed
              health care capacity. Vice versa, if interventions are not relaxed enough initially (e.g., transmission at 30% of its potential),
              little immunity will be developed in the population, increasing the risk of a major outbreak when the next set of intervention
              is relaxed.</p>
              
              <p>In all cases, however, it takes at least 800 days until all interventions can be safely stopped.
              The level of achieved herd immunity depends on whether the last period of flattening the curve includes a major outbreak,
              which would lead to a higher level of herd immunity due to epidemic overshoot.</p>
              "
            )
          ),
          
          box(
            width = 6,
            status = 'primary',
            solidHeader = TRUE,
            collapsible = TRUE,
            title = 'Contact Tracing',
            HTML(
              "
              <div style='float: right;'>
                <img src='Contact-Tracing.png' width=375 height=500>
              </div>
              
              <p>The key idea of contact tracing is to identify and isolate infectious/symptomatic cases,
              and to identify and quarantine any contacts who are at risk of having been infected (and are potentially still incubating infection),
              stopping further spread of the novel coronavirus.</p>
                  
              
              <p>The success of this exit strategy depends on three parameters: (1) delay between a person becoming infectious and being
              identified and isolated (if at all), (2) the probability of an infected contact of an infectious person being identified and
              quarantined before they turn infectious, (3) the quality of isolation and quarantine and their effects on transmission.</p>
              
              <p>The Figure on the right shows that contact tracing is highly sensitive to the quality of isolation and quarantine
              (percentage reduction in transmission): with a two-day delay of isolating infectious cases and a 60% probability of
              successfully quarantining each of their infected contacts, health care capacity will still be overburdened if isolation
              and quarantine reduce transmission to 60%. With only 10 more percentage points effect on transmission (i.e., 70% reduction)
              healthcare capacity would not be exceeded. Alternatively, a higher fraction of exposed contacts would have to be successfully
              quarantined and/or infectious cases would have to be isolated with less delay.</p>
              
              <p>Note that if successfully implemented, contact tracing does not lead to herd immunity. Instead, this policy needs to be
              in place until we have a vaccine. The constant and low proportion of people who have recovered from COVID-19 implies that
              large outbreaks are possible at any time.</p>
              "
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
            withSpinner(plotOutput('exit_visualisation', height = '600px'), color = '#0dc5c1')
          )
        ),

        column(
          width = 3,
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