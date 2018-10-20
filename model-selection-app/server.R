library(shiny)
library(DT)
library(proxy)
library(caret)
library(reshape2)
library(ggplot2)
library(visNetwork)

shinyServer(function(input, output, session) {
  
  getData <- reactive({
    read.csv(file = "tag_data.csv", row.names = 1) 
  })
  
  getFiltData <- reactive({
    d <- getData()
    if (input$Type=="Regression") {
      d <- d[d$Regression == "1",]
    } else {
      d <- d[d$Classification=="1",]
    }
    updateSelectizeInput(session = session, inputId = "Picked", choices = rownames(d), selected = c())
    d
  })
  
  getSimMat <- reactive({
    D <- proxy::dist(getFiltData(), method = "Jaccard", by_rows = TRUE)
    id <- showNotification(session = session, ui = "Calculation clusters")
    hc <- hclust(D)
    removeNotification(id, session)
    D <- as.matrix(D)[hc$order, hc$order]
    sim <- 1 - D
  })
  
  getSimData <- reactive({
    data.frame(melt(getSimMat(), na.rm = TRUE), stringsAsFactors = FALSE)
  })
  
  output$Table <- DT::renderDataTable({
    DT::datatable(data=getFiltData())  
  })
  
  
  
  output$Network <- renderVisNetwork({
    d <- getSimMat()
    for (c in 1:ncol(d)) {
      for (r in 1:c) {
        d[r,c] <- NA
      }
    }
    d <- melt(d, na.rm = TRUE)
    d <- d[d$value > input$Threshold,]
    names <- unique(c(as.character(d$Var1), as.character(d$Var2)))
    nodes <- data.frame(id=names)
    edges <- data.frame(from= d$Var1, to=d$Var2, value=d$value, stringsAsFactors = FALSE)
    visNetwork(nodes = nodes, edges = edges) %>%
      visIgraphLayout(physics = TRUE) %>% 
      visEdges(shadow = TRUE, smooth=TRUE) %>% 
      visEdges(dashes = TRUE)
  })
  
  output$Similarity <- renderPlot({
    ggplot(data=getSimData(), aes(x=Var1, y=Var2, fill=value), col=topo.colors(12)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(high = "red", limit = c(0,1), space = "Lab", name="Model-type similarity") +
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank()
      )
  })
  
  output$Dissimilar <- DT::renderDataTable({
    d <- getFiltData()
    picked <- d[rownames(d) %in% input$Picked, ]
    if (length(input$Picked) > 2) {
      rows <- caret::maxDissim(a=picked, 
                               b=d, 
                               method = "Jaccard",
                               n = 5)
      suggested <- d[rows,]
      alt <- rbind(picked, suggested)
      return(DT::datatable(data=alt)) 
    } else {
      return(DT::datatable(data=picked)) 
    }
  })
  
  output$Models <- DT::renderDataTable({
    if (!is.null(input$plot_brush)) {
      d <- brushedPoints(df = data.frame(getSimData()), brush = input$plot_brush)
      Model <- unique(c(as.character(d$Var1), as.character(d$Var2)))
      DT::datatable(data = as.data.frame(Model))
    } else if (!is.null(input$plot_click)) {
      d <- nearPoints(df = data.frame(getSimData()), coordinfo = input$plot_click, maxpoints = 1)
      Model <- unique(c(as.character(d$Var1), as.character(d$Var2)))
      DT::datatable(data = as.data.frame(Model))
    }
    
  })
  
})
