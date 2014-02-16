library(shiny)
library(ggplot2)
theme_set(theme_bw(base_size=18))
theme_update(axis.title.y=element_text(angle=90, vjust=0.2))

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {
  
  output$myPlot <- renderPlot({
    
    # generate an binomial distribution and plot it
    mydat <- data.frame(
      successes = 0:input$n, 
      probs = dbinom(c(0:input$n), input$n, input$p))
    
    plot1 <- ggplot(mydat, aes(x=successes, y=probs))+
      geom_bar(stat='identity')+
      labs(x='Outcome \n (# of Successes Observed)', y='Probability of Outcome')+
      ggtitle(    paste('Binomial Distribution With n =', input$n, 'and p =', input$p)
)
    print(plot1)
  })
})