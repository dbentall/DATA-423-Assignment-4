library(shiny)
library(DT)

shinyUI(fluidPage(
  
  titlePanel("Caret algorithms"),

  tabsetPanel(
    tabPanel("Data",
             radioButtons("Type", "Type of algorithm:", choices = c("Regression","Classification"), selected = "Regression"),
             DT::dataTableOutput("Table")
    ),
    #        tabPanel("Graph", 
    #                 plotOutput("network")
    #        ),
    tabPanel("Similarity", 
             plotOutput("Similarity", width = 700, height=700)
    ),
    tabPanel("Dissimilar", 
             selectizeInput( inputId="Picked", label="Choosen Model Types", choices = NULL, multiple=TRUE),
             tags$h2("Recommended Model types:"),
             DT::dataTableOutput("Dissimilar")
    )
  )
))
