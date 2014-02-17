library(shiny)
library(ggplot2)
theme_set(theme_bw(base_size=18))
theme_update(axis.title.y=element_text(angle=90, vjust=0.2))

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {
  
  
  output$myPlot <- renderPlot({
    
    if(input$distribution == 'Binomial'){
      
      # generate a binomial distribution
      mydat <- data.frame(
        successes = 0:input$n, 
        probs = dbinom(c(0:input$n), input$n, input$p))
      
      plot1 <- ggplot(mydat, aes(x=successes, y=probs))+
        geom_bar(stat='identity')+
        labs(x='Outcome \n (# of Successes Observed)', y='Probability of Outcome')+
        ggtitle(paste('Binomial Distribution With n =', input$n, 'and p =', input$p))
      
    }else if(input$distribution == 'Poisson'){
      
      # Calculate the largest x to display on the plot (any x with p(x) < .0001 is too large to plot)
      max.x <- ceiling(input$lambda) # start at lambda and work up to find when p(x) gets too small
      myprob <- 1 # start at 1 and work down until p is small
      while(myprob > .0001){
        myprob <<- dpois(max.x, input$lambda)
        max.x <<- max.x+1 # increment max.x by 1 until it's prob is too small to plot
      }
      mydat <- data.frame(
        events = 0:max.x, 
        probs = dpois(c(0:max.x), input$lambda)
        )
      
      plot1 <- ggplot(mydat, aes(x=events, y=probs))+
        geom_bar(stat='identity')+
        labs(x='Outcome \n (# of Events Observed)', y='Probability of Outcome')+
        ggtitle(paste('Poisson Distribution With Lambda = ', input$lambda))
    }
    print(plot1)
  })
})