library(shiny)
library(ggplot2)

# Define UI for application that plots binomial distributions 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Binomial Distribution"),
  
  # Sidebar with a slider input for n and p
  sidebarPanel(
    sliderInput("n", 
                "Number of Trials:", 
                min = 2,
                max = 25, 
                value = 10),
    sliderInput("p", 
                "Probability of Success:", 
                min = 0,
                max = 1,
                step = .01,
                value = .5, 
                animate=animationOptions(interval=300, loop=T))
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    tabsetPanel(
      tabPanel('Binomial', plotOutput("myPlot")),
      tabPanel('Poisson', plotOutput('myPlot2'))
      )
  )
))