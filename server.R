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
    f1 <- input$csvfile1
    if (is.null(f1))
      return(NULL)
    f2 <- input$csvfile2
    if (is.null(f2))
      return(NULL)
    data1 <- read.csv(f1$datapath, header=FALSE, sep=",", quote="", dec=".", fill=FALSE, comment.char="")
    data2 <- read.csv(f2$datapath, header=FALSE, sep=",", quote="", dec=".", fill=FALSE, comment.char="")
    plot(ecdf(data1), col="blue")
    plot(ecdf(data2), add=TRUE, col="red")
  })
  output$text <- renderUI({
    f1 <- input$csvfile1
    if (is.null(f1))
      return(NULL)
    f2 <- input$csvfile2
    if (is.null(f2))
      return(NULL)
    data1 <- read.csv(f1$datapath, header=FALSE, sep=",", quote="", dec=".", fill=FALSE, comment.char="")
    data2 <- read.csv(f2$datapath, header=FALSE, sep=",", quote="", dec=".", fill=FALSE, comment.char="")
    ksdata <- ks.test(as.numeric(data1), as.numeric(data2))
    HTML(paste("Results of the test",
          sprintf("D: %s", format(ksdata[1], digits=5)),
          sprintf("p-value: %s",  format(ksdata[2], digits=5)),
          sep='<br/>'))
  })
})
