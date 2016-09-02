## server.R
## this is the background for the
## Discrete Bayes' applet for Non-Uniform Prior

library(shiny)
library(LearnBayes)

shinyServer(function(input,output) {
  ## reading the CSV file from the input
  prior <- eventReactive(input$prior_file, {
    inFile <- input$prior_file
    if (is.null(inFile)) {
      return (NULL)
    }
    read.csv(inFile$datapath,header = T,sep = ',')
  })
  
  ## setting the population distribution from the input
  distbn <- eventReactive(input$distribution, {
    if (input$distribution == "binom") {
      "binom"
    }
    else if (input$distribution == "pois") {
      "pois"
    }
  })
  
  ## this code creates the 4 plots to be displayed
  output$plot <- renderPlot({
    ## create the 2x2 template and set the window size
    par(mfrow = c(2,2))
    op <- par(mar = c(5,7,4,2) + 0.1)
    ## if/else statements are for the type of distribution for population
    if (distbn() == "binom") {
      # for the prior distribution
      p <- prior()$proportion
      prior <- prior()$prior
      names(prior) <- p
      plot(p,prior,type = 'h',
           col = "cyan4",lwd = 2,
           xlim = c(min(p),max(p)),
           xlab = "Proportion of Successes",
           ylab = "",
           main = "Prior Distribution",
           las = 1)
      mtext(text="Probability", side=2, line=4)
      # plot the population distribution
      x <- seq(0,input$num_trials)
      hx.pop <- dbinom(x,size = length(x) - 1,prob = input$pop_prop)
      plot(x,hx.pop,col = "midnightblue",lwd = 2,type = 'h',
           main = paste0("True Population Distribution\nfor ",input$num_trials," trials"),
           xlab = "Number of Successes",
           ylab = "",
           las = 1)
      mtext(text="Probability", side=2, line=4)
      # plot the prior and posterior on same plot
      posterior <- pdisc(p,prior,
                         c(input$binom_success,input$binom_failures))
      plot(p,posterior,xlim = c(min(p),max(p)),
           ylim = c(0,max(max(prior),max(posterior))),
           main = "Prior vs Posterior Distributions",
           xlab = "Proportion of Successes",
           ylab = "",
           col = "darkorange",
           pch = 20,cex = 1.8,las = 1)
      mtext(text="Probability", side=2, line=4)
      points(p,prior,col = "cyan4",pch = 20,cex = 1.8)
      # plot for the credible interval
      names(posterior) <- p
      d <- discint(cbind(p,posterior),as.numeric(input$cred.int)/100)
      plot(p,posterior,
           xlim = c(min(p),max(p)),
           ylim = c(0,max(max(prior),max(posterior))),
           main = paste0(input$cred.int,"% Credible Interval for Mean"),
           xlab = "Proportion of Successes",
           ylab = "",
           col = "darkorange",
           pch = 20,cex = 1.8,las = 1)
      mtext(text="Probability", side=2, line=4)
      segments(x0 = d$set,y0 = rep(0,length(d$set)),
               y1 = posterior[as.character(d$set)],
               lwd = 2)
    }
    else if (distbn() == "pois") {
      # for the prior
      p <- prior()$value
      prior <- prior()$prior
      names(prior) <- p
      plot(p,prior,
           type = 'h',col = "cyan4",lwd = 2,
           xlim = c(min(p),max(p)),
           xlab = "Mean Number of Occurrences",
           ylab = "",
           main = "Prior Distribution",
           las = 1)
      mtext(text="Probability", side=2, line=4)
      # for the population data
      n <- seq(0,8 + 2*input$pop_lambda)
      hn <- dpois(n,lambda = input$pop_lambda)
      plot(n,hn,pch = 20,col = "midnightblue",type = 'h',lwd = 2,
           main = "Population Distribution of Occurrences",
           xlab = "Occurrences",
           ylab = "Probability",
           las = 1)
      # for the prior vs posterior
      post <- discrete.bayes(dpois,prior,input$pois_sample_n)
      plot(post,col = "darkorange",
           main = "Prior vs Posterior Distributions",
           xlab = "Mean Number of Occurances",
           ylab = "",
           las = 1)
      mtext(text="Probability", side=2, line=4)
      points((p - min(p))*1.2 + 0.7,prior,col = "cyan4",pch = 20,cex = 1.8)
      # for the credible interval
      ## bar.colors allows us to color in the correct bars for the interval
      bar.colors <- function(post.dist) {
        v <- rep("darkorange",length(post.dist$prob))
        hts <- post.dist$prob
        area <- 0
        while (area < as.numeric(input$cred.int)/100) {
          area <- area + max(hts)
          v[which.max(hts)] <- "midnightblue"
          hts[which.max(hts)] <- 0
        }
        return (v)
      }
      plot(post,col = bar.colors(post),
           main = paste0(input$cred.int,"% Credible Interval for Mean"),
           xlab = "Proportion of Successes",
           ylab = "",
           las = 1)
      mtext(text="Probability", side=2, line=4)
    }
  })
  
  ## display the table of data under the plots
  output$table <- renderTable({
    format(prior(), digits = 5)
  })

  ## this will display the actual values of the credible interval
  output$text3 <- renderText({
    if (distbn() == "binom") {
      # for the prior distribution
      p <- prior()$proportion
      prior <- prior()$prior
      names(prior) <- p
      # create the posterior dsitribution
      posterior <- pdisc(p,prior,
                         c(input$binom_success,input$binom_failures))
      names(posterior) <- p
      d <- discint(cbind(p,posterior),as.numeric(input$cred.int)/100)
      paste0(input$cred.int,"% Credible Interval: ",min(d$set)," to ",
             max(d$set),"           ")
    }
    else if (distbn() == "pois") {
      ### this is for the credible interval words written...
      # for the prior distribution
      p <- prior()$value
      prior <- prior()$prior
      names(prior) <- p
      # create posterior dist.
      post <- discrete.bayes(dpois,prior,input$pois_sample_n)
      # for the credible interval      
      bar.colors <- function(post.dist) {
        v <- rep("darkorange",length(post.dist$prob))
        hts <- post.dist$prob
        area <- 0
        while (area < as.numeric(input$cred.int)/100) {
          area <- area + max(hts)
          v[which.max(hts)] <- "midnightblue"
          hts[which.max(hts)] <- 0
        }
        return (v)
      }
      cols <- bar.colors(post)
      paste0(input$cred.int,"% Credible Interval: ",min(which(cols == "midnightblue")) + min(p) - 1,
             " to ",max(which(cols == "midnightblue")) + min(p) - 1)
    }
  })
  
  ## this text acts as a legend for the plots
  output$text1 <- renderText({
    paste0("Blue represents the prior distribution")
  })
  output$text2 <- renderText({
    paste0("Orange represents the posterior distribution")
  })
})




