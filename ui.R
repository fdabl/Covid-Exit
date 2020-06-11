library('shiny')
library('shinyjs')
library('shinythemes')
library('shinydashboard')
library('shinycssloaders')
library('dashboardthemes')


IMGWIDTH <- '100%'
IMGHEIGHT <- '500px'

sidebar <- dashboardSidebar(
  width = 350,
  sidebarMenu(
    menuItem('Overview of Exit Strategies', tabName = 'introduction', icon = icon('file-alt')),
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
      tags$style(type="text/css", ".explanation_plot{text-align: center;} p{font-size: 120%;}"),
      tags$style(type="text/css", "p{font-size: 130%;}"),
      tags$style(type="text/css", ".control-label{font-size: 130%;}"),
      tags$style(type="text/css", ".shiny-options-group{font-size: 120%;}"),
      tabName = 'introduction',
      fluidPage(
        fluidRow(
          box(
            width = NULL,
            tags$h1(
              'What are the effects of different exit strategies?', align = 'center'
            ),
            tags$br(),
            HTML(
              "
              <p>Most countries are affected by the Covid-19 pandemic and experience rapidly increasing numbers of cases and deaths.
              Many have implemented nationwide stringent control to avoid overburdening the health care system.
              This paralyzes economic and social activities until the availability of a vaccine, which may take years.</p>
              
              <p>Here, we compare several alternative exit strategies that either aim to keep the number of infections as low as possible (e.g., contact tracing),
              or that aim to develop herd immunity without exceeding health care capacity. Comparisons are done in terms of the number infections,
              new and prevalent IC admissions, and the level of herd immunity. In each graph, the time points at which interventions change are indicated by vertical
              dotted lines. Lines before day 0 indicate interventions specific to the Netherlands during the initial lockdown, and lines from day 0 onwards indicate 
              interventions that are specific to the exit strategies. Horizontal dashed lines indicate the maximum health care capacity (first and second panels) or
              the target level of herd immunity (fourth panel). Red open circles represent intensive care data specific to the Netherlands reported by NICE.</p>
              
              <p>This app uses the model described in De Vlas & Coffeng
              (<a href='https://www.medrxiv.org/content/10.1101/2020.03.29.20046011v2' target='_blank'>2020</a>) to explore the
              effects of different exit strategies. You can find a brief description of the exit strategies below.
              Under the tab <i>Interactive Exploration</i> on the left, you can further explore these exit strategies.</p>
              "
            )
          )
        ),
        
        fluidRow(
          box(
            width = NULL,
            status = 'primary',
            solidHeader = TRUE,
            collapsible = TRUE,
            title = 'Radical Opening',
            
            fluidRow(
              column(
                width = 7,
                HTML(
                  "
                  <p>The Radical Opening exit strategy involves a sudden lift of the lockdown to return to business-as-usual.
                  The main benefit of this strategy is that it would swiftly lead to herd immunity,
                  after which the epidemic would die off within less than 100 days. Unfortunately, the costs would be vast.</p>
                  
                  <p>The Figure on the right shows that the number of infections would increase dramatically, leading to a substantial overburdening of the
                  healthcare system (red dashed line). Ethically, this is unacceptable.</p>
                  
                  <p>However, this strategy also has economic consequences that are overlooked by its proponents. Things would not go back to normal
                  immediately. In fact, Pichler et al. (<a href='https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3606984' target='_blank'>2020</a>)
                  estimate that opening all industries would still lead to a 16% reduction of GDP compared to pre-lockdown levels for the UK case.
                  A less radical opening strategy would lead to a roughly 17% reduction; paying with so many lives for so little economic gain is foolhardy.</p>
                  
                  <p>In contrast to other exit strategies discussed here, the radical opening exit strategy does not have additional parameters.</p>
                  "
                )
              ),
              
              column(
                width = 5,
                withSpinner(plotOutput('explanation_radical_opening', width = IMGWIDTH, height = IMGHEIGHT), color = '#0dc5c1')
              )
            )
          )
        ),
        
        fluidRow(
          box(
            width = NULL,
            status = 'primary',
            solidHeader = TRUE,
            collapsible = TRUE,
            title = 'Phased Lift of Control',
            
            fluidRow(
              column(
                width = 7,
                HTML(
                  "
                  <p>The Phased Lift of Control (PLoC) exit strategy lifts the lockdown in different areas at different times.
                  It is described in more detail in De Vlas & Coffeng
              (<a href='https://www.medrxiv.org/content/10.1101/2020.03.29.20046011v2' target='_blank'>2020</a>). Given a certain IC capacity the strategy
                  can be adapted in terms of (a) the number of phases, (b) the time intervals between phases, (c) the level
                  of control measures in areas where they are still in force, and (d) the degree to which an area where control
                  is being lifted is isolated from the rest of the country. Applied to the Netherlands, the idea is to split the country
                  into twenty areas in which the lockdowns are subsequently lifted at regular intervals of 45 days (and one interval of 60 days,
                  see vertical dotted lines).</p>
                  
                  <p>The Figure on the right shows that this strategy can be designed such that it does not overburden the healthcare system,
                  resulting in vastly fewer deaths than the radical opening strategy. However, the design on the right (20 phases and a maximum
                  IC capacity of 108 beds per 1 million) requires roughly 750 days  until all measures can be lifted, and achieves a level of
                  80% immunity. As you can explore in the interactive part of the app, this strategy comes in three main variants.</p>
                  
                  <p>The first variant (“standard”) aims to keep the number of cases in the IC within the limits of the health system. The second variant
                  (“efficient”) has the same aim, but makes better use of IC capacity towards the end of the strategy by shortening the intervals
                  between phases (for this strategy, the IC capacity increases so that the horizontal dashed line does not reflect the limit anymore).
                  The third variant (“optimistic”) is based on the assumption that health care capacity will increase over
                  time, either through reduced risk of IC admission, shorter IC admission, and/or availability of more IC beds. All three variants
                  are based on the assumption that wherever lifting of control occurs, the area is partially isolated, such that transmission to
                  and from that area is reduced by 50% (and returns to 100% when control is lifted in the next area). In addition to the
                  three main variants, the user can explore a grid of values for parameters (a, b, c, d) listed above.</p>
                  "
                )
              ),
              column(
                width = 5,
                withSpinner(plotOutput('explanation_phased_opening', width = IMGWIDTH, height = IMGHEIGHT), color = '#0dc5c1')
              )
            )
          )
        )
      ),
      
      fluidRow(
        box(
          width = 12,
          status = 'primary',
          solidHeader = TRUE,
          collapsible = TRUE,
          title = 'Intermittent Lockdown',
          
          fluidRow(
            column(
              width = 7,
              HTML(
                "
                <p>This exit strategy results in lockdowns whenever the number of infections rise to a particular level.
                When timed correctly, this can in theory avoid the healthcare system becoming overburdened.
                In practice, however, the resulting trends in IC cases are extremely sensitive to the timing and duration of lockdown.
                There is high risk of overshooting or undershooting the maximum healthcare capacity, where the latter would lead
                to herd immunity being reached less quickly.</p>
                    
                <p>In the ideal scenario of perfectly timed intermittent lockdown (green line) there is still a chance that the number of cases will
                exceed the IC capacity. In addition to the ideal scenario, we also present two variants that perform worse due to minor changes
                in parameters; see the Figure on the right.</p>
                
                <p>In the first variant, the first period of release is too long (30 instead of 25 days), leading to case numbers quickly
                exceeding health care capacity (blue line). This in turn leads to an inefficient and slow increase in herd immunity during
                the majority of the strategy, and another epidemic that exceeds the IC capacity at the end. In the second variant, the timing
                of the intermittent lockdown is the same as in the ideal scenario, but people adhere suboptimally to the lockdown measures such
                that transmission is reduced to 30% instead of 25% during each intermittent lockdown (red line). As in the first variant, this
                leads to case numbers exceeding the IC capacity at the start and end of the strategy.</p>
                "
              )
            ),
            
            column(
              width = 5,
              withSpinner(plotOutput('explanation_intermittent_lockdown', width = IMGWIDTH, height = IMGHEIGHT), color = '#0dc5c1')
            )
          )
        )
      ),
      
      fluidRow(
        box(
          width = 12,
          status = 'primary',
          solidHeader = TRUE,
          collapsible = TRUE,
          title = 'Flattening the Curve',
          
          fluidRow(
            column(
              width = 7,
              HTML(
                "
                <p>The concept of “flattening the curve” has become widely known. This exit strategy proposes to balance the number
                of infections such that the healthcare system does not become overburdened. A key parameter here is how strongly
                transmission is reduced by interventions.</p>
                  
                <p>The Figure on the right shows three different scenarios in which transmissions are reduced to 30%, 35%, or
                40% in the first period. This illustrates the delicate balance required to make this exit strategy work: in the
                first period after lockdown, interventions would have to be relaxed such that transmission resumes at 35% of its potential level (green line);
                this specification is ideal, but the overall exit strategy is not very robust.
                
                Specifically, if interventions are relaxed too quickly too early (e.g., transmission at 40% of its potential),
                the number of cases will exceed health care capacity in the first as well as last period (blue line).
                Vice versa, if interventions are not relaxed enough initially (e.g., transmission at 30% of its potential),
                little immunity will be developed in the population, increasing the risk of a major outbreak when the next set of intervention
                is relaxed (red line).</p>
                
                <p>In all cases, however, it takes at least 800 days until all interventions can be safely stopped.
                The level of achieved herd immunity depends on whether the last period of flattening the curve includes a major outbreak,
                which would lead to a higher level of herd immunity due to epidemic overshoot.</p>
                "
              )
            ),
            
            column(
              width = 5,
              withSpinner(plotOutput('explanation_flattening_curve', width = IMGWIDTH, height = IMGHEIGHT), color = '#0dc5c1')
            )
          )
        )
      ),
      
      fluidRow(
        box(
          width = 12,
          status = 'primary',
          solidHeader = TRUE,
          collapsible = TRUE,
          title = 'Contact Tracing',
          
          fluidRow(
            column(
              width = 7,
              HTML(
                "
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
            ),
            
            column(
              width = 5,
              withSpinner(plotOutput('explanation_contact_tracing', width = IMGWIDTH, height = IMGHEIGHT), color = '#0dc5c1')
            )
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
              <p>The model follows the SEIR structure and is described in De Vlas & Coffeng
              (2020, <a href='https://www.medrxiv.org/content/10.1101/2020.03.29.20046011v2.supplementary-material' target='_blank'>Suppl. 1</a>).
              Here, we briefly summarize the main points.</p>
              
              <p>First, the model assumes that the population is composed of a susceptible, exposed (i.e., latent infection),
              infectious, and removed subpopulations as illustrated in the Figure on the right. Crucially, it assumes that
              reinfections do not occur, that is, that immunity lasts for life, or at least until the end of the simulation.</p>
              
              <div style='float: right; padding-left: 25px;'>
                <img src='SEIR.png' width='600px' height = '275px'>
              </div>
              
              <p>Second, individuals are assumed to live in clusters (e.g., villages, neighborhoods), which are part of superclusters
              (e.g., larger urban areas, provinces), which together make up a region or country. The model allows for heterogeneity
              among contact rates of individuals and a preferential mixing of individuals with similar contact behaviour
              (i.e., assortative mixing), leading to some clusters having higher or lower average contact rates than the population as a
              whole on average. Interventions can be simulated at both the level of the whole population and at the level of superclusters,
              where the geographical boundaries of implementation units for interventions do not necessarily have to match the geographical
              boundaries of transmission units.</p>
              
              <p>Third, for now the model does not explicitly simulate age or mortality (for the sake of simplicity). Therefore, as
              long as model-predicted trends in prevalent IC cases remain under the maximum IC capacity (dashed horizontal line in
              the third panel), the number of deaths can be considered to be proportional to the number of people that experienced
              infection, which can be deduced from the fraction of the population that has “recovered” from infection (bottom panel).
              For scenarios in which the number of prevalent cases requiring IC exceeds the IC capacity, mortality should be expected
              to be far more than proportional to the number of individuals that have experienced infection.</p>
              
              <p>The model allows simulation of two broad categories of interventions: (1) interventions that reduce transmission for all
              individuals and (2) contact tracing leading to isolation of infectious cases and quarantining of exposed contacts.
              These two modalities can be simulated at the same time and can change in intensity and duration throughout the simulation.
              The first modality, overall reductions in transmission, is simulated at the level of implementation units (superclusters),
              and allows for partial uptake of interventions as well as inter-individual variation in participation / adherence to
              interventions in case of uptake. The second modality, contact tracing, involves two processes. The first is that
              infectious / symptomatic individuals are at a continuous (user-defined) “risk” of being identified and isolated.
              The second is the (user-defined) probability that an exposed individual is detected and quarantined before they become
              infectious. Isolated and quarantined individuals contribute to and are exposed to transmission to a (user-defined) lesser
              degree than the rest of the population. As the model does not explicitly simulate transmission chains (i.e., who infects whom),
              exposed and infectious cases are identified in a random and independent fashion.</p>
              "
          )
        )
      ),
      
      fluidRow(
        box(
          status = 'primary',
          solidHeader = TRUE,
          collapsible = TRUE,
          title = 'Model Quantification',
          width = 12,
          HTML(
            "
              <p>Both the latency time and duration of infectiousness are assumed to be 5 days, where the first follows a
              Weibull distribution with shape 20 and the second follows an exponential distribution. Given the assumed duration
              of infectiousness, we set the transmission rate to 0.48, such that the basic reproduction number R<sub>0</sub> is 2.4. We
              further assume that 90% of transmission happens at the level of clusters, 5% at the level of superclusters,
              and the remaining 5% at the population level. These assumptions result in a doubling time of about 5-7 days during the
              initial phase of epidemic.</p>
              
              <p>The risk of IC admission was estimated based on the number of new and prevalent IC cases in the Netherlands,
              and the distribution of admission durations (NICE), combined with the reported seroprevalence of ~3% in
              blood donors (~21 days after lockdown, Sanquin) and 3.6% in the general population
              (~36 days after lockdown, PIENTER study, RIVM). Based on these data, we estimated that 0.95% of the Dutch
              population had already been infected at the start of lockdown (12 March), and that IC admission occurs in
              1 / 182 cases of infection (assuming an average delay of 14 days since onset of infectiousness).
              The duration of IC admission was estimated at 19.8 days on average (Weibull with shape 1.24).</p>
              "
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
            HTML(
            '
            <p>
              As discussed in the Overview of Exit Strategies, all strategies except Radical Opening have
              parameters that determine how successful they are. Here, you can compare up to two different
              exit strategies as well as tweak their parameters. We recommend not comparing more than five variants, as
              this quickly becomes unwieldy and can lead to disconnections with the server.
            </p>
            '
            ),
            br(),
            
            checkboxGroupInput(
              'exit',
              'Type of Exit Strategy',
              inline = FALSE,
              choices = c(
                'Radical Opening' = 'radical-opening',
                'Phased Lift of Control' = 'phased-opening',
                'Intermittent Lockdown' = 'intermittent-lockdown',
                'Flattening the Curve' = 'flattening-curve',
                'Contact Tracing' = 'contact-tracing'
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
            withSpinner(plotOutput('exit_visualisation', width = '100%', height = '600px'), color = '#0dc5c1')
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
          h2('About', align = 'center'),
          HTML(
            "<p style = 'text-align: center;'>
            This web interface was developed by <a href='https://twitter.com/fdabl' target='_blank'>Fabian Dablander</a>
            and <a href='https://twitter.com/luc_coffeng' target='_blank'>Luc Coffeng</a> as a
            <a href='http://scienceversuscorona.com/' target='_blank'>Science versus Corona</a> project.
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