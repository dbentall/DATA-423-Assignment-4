library(shiny)
library(DT)
library(caret)
library(doParallel)
library(dplyr)
library(ggplot2)

library(glmnet)
library(pls)
library(nnet)
library(gam)
library(kernlab)

clus <- makeCluster(detectCores(all.tests = FALSE, logical = TRUE))
registerDoParallel(clus)  # this will work on windows
onStop(function() stopCluster(clus)) # stop the cluster and free memory on app close

trControl <- trainControl("cv", number = 20, timingSamps = 10)  # shared cross validation specification

shinyServer(function(input, output, session) {
  
  
  getData <- reactive({
    data <- read.csv(file="Ass4Data.csv")
    rownames(data) <- data$ID
    data$ID <- NULL
    data
  })
  
  output$BoxPlots <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    boxplot(d[,numeric], outline=TRUE, main="Boxplot using multiplier of 1.5")
  })
  
  output$DataSummary <- renderPrint({
    str(getData())
  })

  output$Table <- DT::renderDataTable({
    d <- getData()
    numeric <- c(FALSE, sapply(d, is.numeric)) # never round rownames which are the first column (when shown)
    DT::datatable(d) %>%
      formatRound(columns = numeric, digits = 3)
  })
  
  
  ############################################################################## 
  getSplit <- reactive({
    createDataPartition(y = getData()$Y, p = input$Split, list = FALSE)
  })
  
  getTrainData <- reactive({
    getData()[getSplit(),]
  })
  
  getTestData <- reactive({
    getData()[-getSplit(),]
  })
  
  output$SplitSummary <- renderPrint({
    cat(paste("Training observations:", nrow(getTrainData()), "\n", "Testing observations:", nrow(getTestData())))
  })

  
  ##############################################################################  
  getGlmModels <- reactive({
    method <- "glmnet"
    showNotification(id = method, paste("Optimising", method, "hyper-parameters using cross validation"), session = session, duration = NULL)
    mods <- train(Y ~ ., data = getTrainData(), method = method, metric = "RMSE",
                  trControl = trControl,
                  tuneGrid = expand.grid(alpha = seq(0,1, 0.1), lambda = seq(0.1, 10, by = 0.1))
    ) # note glmnet does not support parameter "allowParallel"
    removeNotification(id=method)
    mods
  })
  
  output$GlmModelSummary1 <- renderTable({
    mods <- getGlmModels()
    as.data.frame(mods$bestTune)
  })  
  
  output$GlmModelPlots <- renderPlot({
    mods <- getGlmModels()
    plot(mods$finalModel)
  })     
  
  output$GlmModelSummary2 <- renderPrint({
    print(getGlmModels())
  })
  
  
  ##############################################################################
  getPlsModels <- reactive({
    method <- "pls"
    showNotification(id = method, paste("Optimising", method,"hyper-parameters using cross validation"), session = session, duration = NULL)
    mods <- train(Y ~ ., data = getTrainData(), method = method, metric = "RMSE", scale = FALSE, 
                  trControl = trControl,
                  tuneGrid = expand.grid(ncomp = seq(1, 10, by = 1)),
                  allowParallel = TRUE
                  )
    
    removeNotification(id=method)
    mods
  })
  
  output$PlsModelSummary1 <- renderTable({
    mods <- getPlsModels()
    as.data.frame(mods$bestTune)
  })  
  
  output$PlsModelPlots <- renderPlot({
    plot(getPlsModels())
  })     
  
  output$PlsModelSummary2 <- renderPrint({
    mods <- getPlsModels()
    summary(mods$finalModel)
  })
  
  
  
  ##############################################################################
  getAnnModels <- reactive({
    method <- "nnet"
    showNotification(id = method, paste("Optimising", method, "hyper-parameters using cross validation"), session = session, duration = NULL)
    mods <- caret::train(Y ~ ., data = getTrainData(), method = method, metric = "RMSE", maxit = 1000, trace = F, linout = 1,
                  trControl = trControl,
                  tuneGrid = expand.grid(.decay = seq(0.3, 0.6, by=0.1), .size = seq(4, 8, by=1)),
                  allowParallel = TRUE
                  ) 
    removeNotification(id=method)
    mods
  })
  
  output$AnnModelSummary1 <- renderTable({
    mods <- getAnnModels()
    as.data.frame(mods$bestTune)
  })  
  
  output$AnnModelPlots <- renderPlot({
    plot(getAnnModels())
  })     
  
  output$AnnModelSummary2 <- renderPrint({
    mods <- getAnnModels()
    print(mods$finalModel)
  })
  
  
  
  ##############################################################################
  getGAMModels <- reactive({
    method <- "gamSpline"
    showNotification(id = method, paste("Optimising", method, "hyper-parameters using cross validation"), session = session, duration = NULL)
    mods <- caret::train(Y ~ ., data = getTrainData(), method = method, metric = "RMSE",
                         trControl = trControl,
                         tuneGrid = expand.grid(df = seq(1, 3))
    ) 
    removeNotification(id=method)
    mods
  })
  
  output$GAMModelSummary1 <- renderTable({
    mods <- getGAMModels()
    as.data.frame(mods$bestTune)
  })  
  
  output$GAMModelPlots <- renderPlot({
    plot(getGAMModels())
  })     
  
  output$GAMModelSummary2 <- renderPrint({
    mods <- getGAMModels()
    print(mods$finalModel$coefficients)
  })
  
  
  ##############################################################################
  getSVMModels <- reactive({
    method <- "svmRadial"
    showNotification(id = method, paste("Optimising", method, "hyper-parameters using cross validation"), session = session, duration = NULL)
    mods <- caret::train(Y ~ ., data = getTrainData(), method = method, metric = "RMSE",
                         trControl = trControl,
                         tuneGrid = expand.grid(C = 10^seq(4,7), sigma = 10^seq(-2,-5))
    ) 
    removeNotification(id=method)
    mods
  })
  
  output$SVMModelSummary1 <- renderTable({
    mods <- getSVMModels()
    as.data.frame(mods$bestTune)
  }, digits = -1)  
  
  output$SVMModelPlots <- renderPlot({
    plot(getSVMModels())
    df <- getSVMModels()$results
    df$Cost <- factor(df$C)
    ggplot(df) +
      geom_line(aes(sigma, RMSE, col=Cost)) +
      geom_point(aes(sigma, RMSE, col=Cost)) + 
      scale_x_continuous(trans='log10') + 
      coord_cartesian(ylim = c(10, 60)) +
      ylab("RMSE (Cross-Validation")
  })
  
  output$SVMModelSummary2 <- renderPrint({
    mods <- getSVMModels()
    print(mods$finalModel)
  })
  
  
  ##############################################################################
  getkNNModels <- reactive({
    method <- "knn"
    showNotification(id = method, paste("Optimising", method, "hyper-parameters using cross validation"), session = session, duration = NULL)
    mods <- caret::train(Y ~ ., data = getTrainData(), method = method, metric = "RMSE",
                         trControl = trControl,
                         tuneGrid = expand.grid(k = seq(1, 20))
    ) 
    removeNotification(id=method)
    mods
  })
  
  output$kNNModelSummary1 <- renderTable({
    mods <- getkNNModels()
    as.data.frame(mods$bestTune)
  })  
  
  output$kNNModelPlots <- renderPlot({
    plot(getkNNModels())
  })
  
  output$kNNModelSummary2 <- renderPrint({
    mods <- getkNNModels()
    print(mods$finalModel)
  })
  
  
  ##############################################################################  
  getAllModels <- reactive({
    list(GLMnet=getGlmModels(), PLS=getPlsModels(), ANN=getAnnModels(), GAM=getGAMModels(), SVM=getSVMModels(), kNN=getkNNModels())
  })
  
  output$SelectionSummary <- renderPrint({
    results <- resamples(getAllModels())
    summary(results)
  })
  
  output$SelectionBoxPlot <- renderPlot({
    results <- caret::resamples(getAllModels())
    bwplot(results, notch=input$Notch, scales = "free")
  })
  
  output$Times <- renderPrint({
    cat("Final model processing times (s):\n")
    print(dplyr::select(resamples(getAllModels())$timings, Training=FinalModel, Predicting=Prediction))
  })
  
  
  ############################################################################## 
  output$Title <- renderUI({
    tags$h3(paste("Unseen data results for chosen model:", input$Choice))
  })

  getTestResults <- reactive({
    test <- getTestData()
    mod <- getAllModels()[input$Choice]
    predictions <- predict(mod, newdata=test)
    d <- data.frame(test$Y, predictions)
    colnames(d) <- c("obs", "pred")
    d
  })
  
  output$TestSummary <- renderPrint({
    caret::defaultSummary(getTestResults())
  })

  output$TestPlot <- renderPlot({
    plot(getTestResults(), main="Predicted versus Observed")
    abline(a = 0, b=1)
  })
  
})
