library(shiny)
library(ggplot2)

# Define UI for application that plots binomial distributions 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Distribution Calculator"),
  
  # Sidebar with a slider input for n and p
  sidebarPanel(
    # Choose Distribution
    selectInput('distribution', 
                'Choose Distribution',
                c('Binomial', 'Poisson')),
    # Binomial Inputs
    conditionalPanel(
      condition="input.distribution == 'Binomial'",
      # input binomial parameter "n"
      sliderInput("n", 
                  "Number of Trials:", 
                  min = 2,
                  max = 25, 
                  value = 10),
      # Input binomial parameter "p"
      sliderInput("p", 
                  "Probability of Success:", 
                  min = 0,
                  max = 1,
                  step = .05,
                  value = .5, 
                  animate=animationOptions(interval=300, loop=T))
    ),
    
    conditionalPanel(
      condition="input.distribution == 'Poisson'",
      sliderInput("lambda",
                  "Lambda",
                  min = 1,
                  max = 15)
    )
    
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("myPlot")
  )
))