## ui.R
## the user interface for the 
## Discrete Bayes' applet for Non-Uniform Prior

shinyUI(fluidPage(
  ## App Title
  h1("DISCRETE BAYESIAN STATISTICS"),
  h3("WITH A DISCRETE NON-UNIFORM PRIOR DISTRIBUTION"),
  
  ## the sidepanel for the inputs
  sidebarPanel(
    ## input a file for the prior distribution
    fileInput('prior_file','Input Your Prior Distribution',
              accept = c(
                'text/csv',
                'text/comma-separated-values',
                'text/tab-separated-values',
                'text/plain',
                '.csv',
                '.tsv'
              )
    ),

    ## buttons to set up type of distribution the population follows
    radioButtons(inputId = "distribution",label = "Population Distribution to Pick From",
                 choices = c("Binomial" = "binom","Poisson" = "pois")),
    br(),
    ## panel to set population parameters for Binomial dist.
    conditionalPanel(
      condition = "input.distribution == 'binom'",
      h4("Binomial Population Distribution"),
      numericInput("num_trials","Number of Trials",value = 10,min = 1),
      numericInput("pop_prop","True Probability of Success",value = 0.5,min = 0,max = 1),
      br(),
      h4("Results from Sample"),
      numericInput("binom_success","Number of Successes in Sample",value = 0,
                   min = 0),
      numericInput("binom_failures","Number of Failures in Sample",value = 0,min = 0)
    ),
    ## panel to set population parameters for Poisson dist.
    conditionalPanel(
      condition = "input.distribution == 'pois'",
      h4("Poisson Population Distribution"),
      numericInput("pop_lambda","True Population Mean",value = 4,min = 1),
      br(),
      h4("Results from Sample"),
      numericInput("pois_sample_n","Number of Occurrences in Sample",value = 0,min = 0)
    ),
    br(),
    radioButtons(inputId = "cred.int",label = "Size of Credible Interval",
                   choices = c("90%" = "90","95%" = "95","99%" = "99")),
    br(),
    h6("Made by Hans Schumann")
    
  ),
  mainPanel(
    plotOutput('plot'),
    h6(textOutput("text3"),align = "right"),
    textOutput("text1"),
    textOutput("text2"),
    tags$head(
      tags$style(
        "#text1{font-size: 11px;}",
        "#text2{font-size: 11px;}"
      )
    ),
    h5("The Prior Distribution Table"),
    tableOutput('table')
  )
))






