#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  output$cvsdata1 <- renderTable({
    f1 <- input$csvfile1
    if (is.null(f1))
      return(NULL)
    read.csv(f1$datapath, header=FALSE, sep=",", quote="", dec=".", fill=FALSE, comment.char="")
  })
  output$cvsdata2 <- renderTable({
    f2 <- input$csvfile2
    if (is.null(f2))
      return(NULL)
    read.csv(f2$datapath, header=FALSE, sep=",", quote="", dec=".", fill=FALSE, comment.char="")
  })
  output$plot <- renderPlot({
    c <- input$choiceRb
    f1 <- input$csvfile1
    if (is.null(f1))
      return(NULL)
    data1 <- read.csv(f1$datapath, header=FALSE, sep=",", quote="", dec=".", fill=FALSE, comment.char="")
    if (c == "0"){
      f2 <- input$csvfile2
      if (is.null(f2))
        return(NULL)
      data2 <- read.csv(f2$datapath, header=FALSE, sep=",", quote="", dec=".", fill=FALSE, comment.char="")
      plot(ecdf(data1), col="blue")
      plot(ecdf(data2), add=TRUE, col="red")
    }else{
      data2 <- switch(
        input$df,
        "Beta" = rbeta(as.numeric(data1), as.numeric(input$parameter1), as.numeric(input$parameter2)),
        "Binomial" = rbinom(as.numeric(data1), as.numeric(input$parameter1), as.numeric(input$parameter2)),
        "Cauchy" = rcauchy(as.numeric(data1), as.numeric(input$parameter1), as.numeric(input$parameter2)),
        "Chi-Square" = rchisq(as.numeric(data1), as.numeric(input$parameter1)),
        "Exponential" = rexp(as.numeric(data1), as.numeric(input$parameter1)),
        "F" = rf(as.numeric(data1), as.numeric(input$parameter1), as.numeric(input$parameter2)),
        "Gamma" = rgamma(as.numeric(data1), as.numeric(input$parameter1), as.numeric(input$parameter2)),
        "Geometric" = rgeom(as.numeric(data1), as.numeric(input$parameter1)),
        "Hypergeometric" = rhyper(as.numeric(data1), as.numeric(input$parameter1), as.numeric(input$parameter2), as.numeric(input$parameter3)),
        "Logistic" = rlogis(as.numeric(data1), as.numeric(input$parameter1), as.numeric(input$parameter2)),
        "Log normal" = rlnorm(as.numeric(data1), as.numeric(input$parameter1), as.numeric(input$parameter2)),
        "Negative Binomial" = rnbinom(as.numeric(data1), as.numeric(input$parameter1), as.numeric(input$parameter2), as.numeric(input$parameter3)),
        "Normal" = rnorm(as.numeric(data1), as.numeric(input$parameter1), as.numeric(input$parameter2)),
        "Poisson" = rpois(as.numeric(data1), as.numeric(input$parameter1)),
        "Student" = rt(as.numeric(data1),as.numeric(input$parameter1)),
        "Stuendentized Range" = rtukey(as.numeric(data1), as.numeric(input$parameter1), as.numeric(input$parameter2)),
        "Uniform" = runif(as.numeric(data1), as.numeric(input$parameter1), as.numeric(input$parameter2)),
        "Weibull" = rweibull(as.numeric(data1), as.numeric(input$parameter1), as.numeric(input$parameter2)),
        "Wilcoxon Rank Sum Statistic" = rwilcox(as.numeric(data1), as.numeric(input$parameter1), as.numeric(input$parameter2)),
        "Wilcoxon Signed Rank Statistic" = rsignrank(as.numeric(data1), as.numeric(input$parameter1))
      )
      plot(ecdf(data1), col="blue")
      plot(ecdf(data2), add=TRUE, col="red")
    }
    
    
  })
  output$text <- renderUI({
    c <- input$choiceRb
    f1 <- input$csvfile1
    if (is.null(f1))
      return(NULL)
    data1 <- read.csv(f1$datapath, header=FALSE, sep=",", quote="", dec=".", fill=FALSE, comment.char="")
    if (c == "0"){
      f2 <- input$csvfile2
      if (is.null(f2))
        return(NULL)
      data2 <- read.csv(f2$datapath, header=FALSE, sep=",", quote="", dec=".", fill=FALSE, comment.char="")
      ksdata <- ks.test(as.numeric(data1), as.numeric(data2))
      HTML(paste("Results of the test",
                 sprintf("D: %s", format(ksdata[1], digits=5)),
                 sprintf("p-value: %s",  format(ksdata[2], digits=5)),
                 sep='<br/>'))
    }else{
      if(input$df != "None"){
        distrib <- switch(
          input$df,
          "Beta" = "pbeta",
          "Binomial" = "pbinom",
          "Cauchy" = "pcauchy",
          "Chi-Square" = "pchisq",
          "Exponential" = "pexp",
          "F" = "pf",
          "Gamma" = "pgamma",
          "Geometric" = "pgeom",
          "Hypergeometric" = "phyper",
          "Logistic" = "plogis",
          "Log normal" = "plnorm",
          "Negative Binomial" = "pnbinom",
          "Normal" = "pnorm",
          "Poisson" = "ppois",
          "Student" = "pt",
          "Stuendentized Range" = "ptukey",
          "Uniform" = "punif",
          "Weibull" = "pweibull",
          "Wilcoxon Rank Sum Statistic" = "pwilcox",
          "Wilcoxon Signed Rank Statistic" = "psignrank"
        )
        nbParameters <- as.numeric(datasetInput2())
        if(nbParameters == 0){
          ksdata <- ks.test(as.numeric(data1), distrib)
        }
        if(nbParameters == 1){
             ksdata <- ks.test(as.numeric(data1), distrib, as.numeric(input$parameter1))
        }
        if(nbParameters == 2){
             ksdata <- ks.test(as.numeric(data1), toString(distrib), 
                               as.numeric(input$parameter1),
                               as.numeric(input$parameter2))   
        } 
        if(nbParameters == 3){
             ksdata <- ks.test(as.numeric(data1), toString(distrib), 
                               as.numeric(input$parameter1),
                               as.numeric(input$parameter2),
                               as.numeric(input$parameter3))   
        }  
        
        HTML(paste("Results of the test",
                   sprintf("D: %s", format(ksdata[1], digits=5)),
                   sprintf("p-value: %s",  format(ksdata[2], digits=5)),
                   sep='<br/>'))
      }
    }
  })
  
  datasetInput <- reactive({
    switch(input$choiceRb,
           "0" = TRUE,
           "1" = FALSE)
  })
  
  output$choice <- reactive({
    datasetInput()
  })
  
  outputOptions(output, "choice", suspendWhenHidden = FALSE)  
  
  datasetInput2 <- reactive({
    switch(input$df,
           "Beta" = 2,
           "Binomial" = 2,
           "Cauchy" = 2,
           "Chi-Square" = 1,
           "Exponential" = 1,
           "F" = 2,
           "Gamma" = 2,
           "Geometric" = 1,
           "Hypergeometric" = 3,
           "Logistic" = 2,
           "Log normal" = 2,
           "Negative Binomial" = 3,
           "Normal" = 2,
           "Poisson" = 1,
           "Student" = 1,
           "Stuendentized Range" = 2,
           "Uniform" = 2,
           "Weibull" = 2,
           "Wilcoxon Rank Sum Statistic" = 2,
           "Wilcoxon Signed Rank Statistic" = 1)
  })
  
  output$parameters <- reactive({
    datasetInput2()
  })
  
  outputOptions(output, "parameters", suspendWhenHidden = 0)  
  
})
