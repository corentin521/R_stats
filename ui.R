library(shiny)
library(shinythemes)

shinyUI(fluidPage(theme = shinytheme("darkly"),

  titlePanel("Kolmogorov-Smirnov test"),

  fluidRow(
    column(4,
           fileInput("csvfile1", "Choose CSV File",
                     accept = c(
                       "text/csv",
                       ".csv")
           )       
    ),
    column(8,
           tableOutput('cvsdata1')
    )
  ),
  fluidRow(
    column(4,
           fileInput("csvfile2", "Choose CSV File",
                     accept = c(
                       "text/csv",
                       ".csv")
           )      
    ),
    column(8,
           tableOutput('cvsdata2')
    )
  ),
  fluidRow(
    column(12,
           plotOutput('plot')
    )
  ),
  fluidRow(
    column(12,
           htmlOutput('text')
    )
  )
))
