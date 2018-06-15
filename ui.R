
library(shiny)

# Define UI
shinyUI(
  fluidPage(
    
    # title
    titlePanel("Bayesian inference for a population mean"),
    h5('(Design based on slides by @mjskay,',
       a('https://buff.ly/2wKB49g)', href='https://buff.ly/2wKB49g')),
    
    # css styles
    tags$head(
      tags$style(HTML('p{font-size: 16px}')),
      tags$style(HTML('li{font-size: 16px}')),
      tags$style(HTML('#mu_prior{background-color: rgba(27, 158, 119, 0.5);
                                 border-color: rgb(27, 158, 119)}')),
      tags$style(HTML('#sd_prior{background-color: rgba(27, 158, 119, 0.5);
                                 border-color: rgb(27, 158, 119)}')),
      tags$style(HTML('#sample_data{color: white;
                                    background-color: rgba(80, 80, 80, 0.8);
                                    border-color: black; border-width: 1.5px}')),
      tags$style(HTML('#prior{color: #1b9e77; font-weight: bold}')),
      tags$style(HTML('#likelihood{color: #d95f02; font-weight: bold}')),
      tags$style(HTML('#posterior{color: #7570b3; font-weight: bold}')),
      tags$style(HTML('#data{color: #e7298a; font-weight: bold}')),
      tags$style(HTML('#mean{color: #000000}'))
    ),
    
    # layout
    sidebarLayout(
      
      # side panel
      sidebarPanel(
        width = 3,
        numericInput('mu_prior', 'Prior mean', value = 0, step = 0.5),
        numericInput('sd_prior', 'Prior SD', value = 0.7, min = 0.1, step = 0.2),
        br(),
        numericInput('n', 'Sample size', value = 15, step = 10, min = 5),
        numericInput('mu_data', 'True population mean', value = 1.5, step = 0.5),
        numericInput('sd_data', 'True population SD', min = 0, value = 1.5, step = 0.5),
        checkboxInput('axis_lock', 'Lock x-axis', value = TRUE),
        actionButton('sample_data', 'New sample', width = '100%')
      ),
      
      # main panel
      mainPanel(
        width = 9, # main panel takes up 9 of 12 units
        
        # main plot
        plotOutput("distPlot", height = '500px'),
        
        # description
        br(),
        h4('Description'),
        p('The app draws a sample of a random variable y from a Gaussian
          distribution, and illustrates both Bayesian and frequentist methods
          for making inferences about the population mean of y based on the
          sample.'),
        br(),
        h4('Details'),
        p("Imagine we want to make inferences about the population
        mean of some variable (y) based on data from a sample of that population.
        Perhaps y is the percentage change in colony size for a particular
        species of ant over some time period, and we want to know if the
        population mean change in colony size is positive (i.e. if colonies
        are generally increasing in size) based on data from a sample of
        colonies."),
        br(),
        p("In a frequentist analysis our inference will be based on likelihood:",
          tags$ul(
            tags$li(HTML("<span id = 'likelihood'>Likelihood</span> = 
                         P( <span id = 'data'>sample data</span> |
                         <span id = 'mean'>population mean</span> )")
            )
          )
        ),
        br(),
        p("If we instead use Bayesian methods, our inference will be based on
         posterior probability, which is a function of both likelihood and our
         prior beliefs about the population mean change in colony size:",
          tags$ul(
            tags$li(HTML("<span id = 'prior'>Prior</span> = P( population mean )")
            ),
            tags$li(HTML("<span id = 'posterior'>Posterior</span> = 
                         P( <span id = 'mean'>population mean</span> |
                         <span id = 'data'>sample data</span> ) ∝
                         <span id = 'prior'>Prior</span> ×
                         <span id = 'likelihood'>Likelihood</span>")
            )
          )
        ),
        br(),
        p("Here we use Gaussian distributions to model both the prior and
          likelihood."),
        p("Adjust the settings in the sidebar to examine how our inferences may
         be shaped by the choice of inferential method, choice of prior, and the
         properties of the sample. For example, increasing the sample size or
         decreasing the population standard deviation will generally narrow the
         likelihood distribution, which will simultaneously shift the posterior
         toward the likelihood. Making the prior less informative (i.e.
         increasing Prior SD) will also shift the posterior toward the
         likelihood.")
      )
    )
  )
)
