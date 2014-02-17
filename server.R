library(shiny)
library(ggplot2)
theme_set(theme_bw(base_size=18))
theme_update(axis.title.y=element_text(angle=90, vjust=0.2))

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {
  
  output$binom.x <- renderUI({
    numericInput("binomx",
                 "",
                 min = 0,
                 max = input$n,
                 step = 1,
                 value = round(input$p*input$n, 0))
  })
  
  output$pois.x <- renderUI({
    pvec <- dpois(c(1:17), input$lambda)
    upLim <- max(which(pvec > .0001))
    numericInput("poisx",
                 "",
                 min = 0,
                 max = upLim,
                 step = 1,
                 value = round(input$lambda, 0))
  })
  
  myexpr.global <- reactive({
    if(input$distribution=='Binomial'){
      myexpr <- paste('mydat$x', input$logicalCond, input$binomx) # get inputs and create expression as a string
    }else if(input$distribution == 'Poisson'){
      myexpr <- paste('mydat$x', input$logicalCond, input$poisx) # get inputs and create expression as a string
    }
    return(myexpr)
  })
  
  mydat.global <- reactive({
    if(input$distribution == 'Binomial'){
      # generate a binomial distribution
      mydat <- data.frame(
        x = 0:input$n, 
        probs = round(dbinom(c(0:input$n), input$n, input$p), 3)
      )
    }else if(input$distribution == 'Poisson'){
      mydat <- data.frame(
        x = 0:17, 
        probs = round(dpois(c(0:17), input$lambda), 4)
      )
      mydat <- subset(mydat, probs > .0001) # truncate to show only x's with p(x) > .0001
    }
    # Highlight values user has selected
    mydat$selected <- F # create column "selected", start by selecting none
    myexpr <- myexpr.global()
    logvec <- eval(parse(text=myexpr)) # parse and evaluate expression string (returns a logical vector)
    mydat$selected[logvec] <- T # those x's that fit the users criteria are now "selected"
    return(mydat)
  })
  
  output$myText <- renderText({
    
    if(input$distribution == 'Binomial'){
      line1 <- paste('Binomial Distribution With n =', input$n, 'and p =', input$p)
      if(input$logicalCond=='<'){
        myval <- pbinom(input$binomx-1, input$n, input$p)
      }else if(input$logicalCond=='<='){
        myval <- pbinom(input$binomx, input$n, input$p)
      }else if(input$logicalCond=='=='){
        myval <- dbinom(input$binomx, input$n, input$p)
      }else if(input$logicalCond=='>='){
        myval <- 1-pbinom(input$binomx-1, input$n, input$p)
      }else if(input$logicalCond=='>'){
        myval <- 1-pbinom(input$binomx, input$n, input$p)
      }
      myval <- round(myval, 3)
      line2 <- paste0("P(x", input$logicalCond, " ", input$binomx, ") =", myval)
    }else if(input$distribution == 'Poisson'){
      line1 <- paste('Poisson Distribution With Lambda =', input$lambda)
      if(input$logicalCond=='<'){
        myval <- ppois(input$poisx-1, input$lambda)
      }else if(input$logicalCond=='<='){
        myval <- ppois(input$poisx, input$lambda)
      }else if(input$logicalCond=='=='){
        myval <- dpois(input$poisx, input$lambda)
      }else if(input$logicalCond=='>='){
        myval <- 1-ppois(input$poisx-1, input$lambda)
      }else if(input$logicalCond=='>'){
        myval <- 1-ppois(input$poisx, input$lambda)
      }
      myval <- round(myval, 4)
      line2 <- paste0("P(x", input$logicalCond, " ", input$poisx, ") =", myval)
    }
    
    return(paste(line1, line2, sep='\n'))
  })
  
  output$myPlot <- renderPlot({
    
    plotdat <- mydat.global()
    
    plot1 <- ggplot(plotdat, aes(x=x, y=probs, fill=selected))+
      geom_bar(stat='identity')+
      geom_text(data=plotdat[plotdat$selected,], aes(label=probs), vjust=(-1))+
      scale_fill_manual(values=c('black', 'red'))+
      scale_y_continuous(limits=c(0, max(plotdat$probs)+.05))+
      guides(fill=F)
    
    if(input$distribution == 'Binomial'){
      plot1 <- plot1 +
        labs(x='Outcome \n (# of Successes Observed)', y='Probability of Outcome', fill='', title='')
    }else if(input$distribution == 'Poisson'){
      plot1 <- plot1 +
        labs(x='Outcome \n (# of Events Observed)', y='Probability of Outcome', fill='', title='')
    }
    print(plot1)
  })
})