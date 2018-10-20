library(shiny)
library(DT)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Assignment 4"),
  tags$h4("Daniel Bentall - 65254210"),
  tags$h4("21/10/18"),
  
  tabsetPanel(
    tabPanel("Data",
             verbatimTextOutput("DataSummary"),
             plotOutput("BoxPlots"),
             DT::dataTableOutput("Table")
    ),
    tabPanel("Split",
             sliderInput("Split", "Train proportion", min = 0, max=1, value = 0.8),
             verbatimTextOutput("SplitSummary")
    ),
    tabPanel("GLMnet Model",
             tags$h3("Best tuning parameters:"),
             tableOutput("GlmModelSummary1"),
             hr(),
             plotOutput("GlmModelPlots"),
             verbatimTextOutput("GlmModelSummary2")
    ),
    tabPanel("PLS Model",
             tags$h3("Best tuning parameters:"),
             tableOutput("PlsModelSummary1"),
             hr(),
             plotOutput("PlsModelPlots"),
             verbatimTextOutput("PlsModelSummary2")
    ),
    tabPanel("ANN Model",
             tags$h3("Best tuning parameters:"),
             tableOutput("AnnModelSummary1"),
             hr(),
             plotOutput("AnnModelPlots"),
             verbatimTextOutput("AnnModelSummary2")
    ),
    tabPanel("GAM Model",
             tags$h3("Best tuning parameters:"),
             tableOutput("GAMModelSummary1"),
             hr(),
             plotOutput("GAMModelPlots"),
             verbatimTextOutput("GAMModelSummary2")
    ),
    tabPanel("SVM Model",
             tags$h3("Best tuning parameters:"),
             tableOutput("SVMModelSummary1"),
             hr(),
             plotOutput("SVMModelPlots"),
             verbatimTextOutput("SVMModelSummary2")
    ),
    tabPanel("kNN Model",
             tags$h3("Best tuning parameters:"),
             tableOutput("kNNModelSummary1"),
             hr(),
             plotOutput("kNNModelPlots"),
             verbatimTextOutput("kNNModelSummary2")
    ),
    tabPanel("Model Selection",
             tags$h3("Cross validation results:"),
             checkboxInput("Notch", "Show notch", value = FALSE),
             plotOutput("SelectionBoxPlot"),
             verbatimTextOutput("Times"),
             sidebarLayout(
               sidebarPanel(width = 3,
                 radioButtons("Choice", "Model choice", choices = c("GLMnet", "PLS", "ANN", "GAM", "SVM", "kNN"), 
                              selected = "SVM")
               ),
               mainPanel(
                 "There are many factors that affect model selection beyond just MAE, RMSE and R-squared, including
                 intelligibility, complexity and model capabilities. Depending on the application these vary in importance.
                 ",br(),br(),"
                 Some models are more intelligible and therefore transparent than others. In critical application
                 areas such as healthcare and law it is important that a model's decisions can be analysed and
                 understood. This is necessary so that biases can be discovered and removed, and decisions can be
                 explained to affected parties. GLMnets, PLSs and GAMs are models that can be expressed in terms 
                 of coefficients and predictors, so the influence of a predictor on the result can be evaluated.
                 A prediction using kNN can be explained by its neighbors. ANNs and SVMs are non-parametric models
                 which are more difficult to interpret.
                 ",br(),br(),"
                 Some models can take a prohibitively long time or a require a large amount of RAM to train, especially 
                 on larger datasets. Others can take a long time to make predictions, which can be a problem in
                 applications requiring real-time performance. Greedy models generally take a long time to train 
                 and make predictions more quickly, while lazy algorithms tend to be the opposite. The times above
                 show that ANNs and SVMs are slow to train while GAMs, and sometimes SVMs and GLMnets, are slow to make predictions. 
                 Some models can split their processing and run in parallel, which allows them to be scaled up
                 easily by deployment on a cluster. All of these models use all available cores of the CPU, as can be 
                 seen in Task Manager.
                 ",br(),br(),"
                 A related aspect is the internal complexity of a model, which is similar to its number of trainable 
                 parameters. A model with greater complexity needs a larger amount of training data to fit the
                 parameters accurately. Since these models are tuned with cross-validation, inherently complex models
                 will not overfit the data, but they will not be able to use their complexity to their advantage. ANNs 
                 have a great number of parameters, so this one may be performing 
                 poorly due to the small dataset size. Interestingly the GAM model chosen by the tuning process
                 is the GAM with the least internal complexity, with only one degree of freedom. This reduces to
                 a linear regression, suggesting that this these predictors generally have linear relationships
                 with the response.
                 ",br(),br(),"
                 Finally, different models offer various features, such as being able to classify and/or perform regression,
                 being tolerant of missing values and outliers and providing prediction confidence intervals 
                 without needing to resample. In the absence of application information the SVM is the clear winner,
                 with its RMSE and MAE less than half that of the other models, and a much higher R-squared. It also has a
                 tighter distribution of these statistics, suggesting that it is more stable and thus will generalise better 
                 to unseen data"
               )
             )
    ),
    tabPanel("Performance",
             htmlOutput("Title"),
             verbatimTextOutput("TestSummary"),
             plotOutput("TestPlot")
    )
  )
))
