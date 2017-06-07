library(shiny)
library(shinythemes)

shinyUI(fluidPage(theme = shinytheme("darkly"),

  titlePanel("Kolmogorov-Smirnov test"),

  fluidRow(
    column(4,
           fileInput("csvfile1", "First sample : choose CSV File",
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
           radioButtons("choiceRb", "Choose an element to compare to the first sample:",
                        choiceNames = list(
                          "Import another sample",
                          "Choose a distribution function"
                        ),
                        choiceValues = list(
                          "0", "1"
                        ))     
    ),
    column(8,
           textOutput('choiceRb')
    )
  ),
  fluidRow(
    column(4, conditionalPanel( condition = "output.choice == true",
                                fileInput("csvfile2", "Choose CSV File",
                                          accept = c(
                                            "text/csv",
                                            ".csv")
                                ) )     
    ),
    column(8, conditionalPanel( condition = "output.choice == true",
                                tableOutput('cvsdata2') ) 
             
           
    )
  ),
  fluidRow(
    column(3, conditionalPanel( condition = "output.choice == false",
                                          selectInput(inputId = "df",
                                                      label = "Distribution function:",
                                                      choices = c('None', 'Beta', 'Binomial', 'Cauchy', 'Chi-Square',
                                                                  'Exponential', 'F', 'Gamma', 'Geometric',
                                                                  'Hypergeometric', 'Logistic', 'Log normal',
                                                                  'Negative Binomial', 'Normal', 'Poisson',
                                                                  'Student', 'Stuendentized Range', 'Uniform',
                                                                  'Weibull', 'Wilcoxon Rank Sum Statistic',
                                                                  'Wilcoxon Signed Rank Statistic'),
                                                      selected = 'None') )   
           
           
           
           
    ),
    column(3,
           conditionalPanel( condition = "output.choice == false && output.parameters >= 1",
                             textInput("parameter1", "First parameter", width = 150) )  
           
    ),
    column(3,
           conditionalPanel( condition = "output.choice == false && output.parameters >= 2",
                             textInput("parameter2", "Second parameter", width = 150) )  
           
    ),
    column(3,
           conditionalPanel( condition = "output.choice == false && output.parameters >= 3",
                             textInput("parameter3", "Third parameter", width = 150) )  
           
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
