source("global.R")
function(input, output, session) {
  
  
  # Density plots with ggplot ----
  output$densPlot <- renderPlotly({
    
    drange <- density(vertebrae.db[,input$dpvar], na.rm = T)
    
    if (input$filter == FALSE) {
      p <- ggplot(vertebrae.db) + aes_string(input$dpvar) + 
        geom_density(kernel = input$kernel, adjust = input$bandwidth) + xlim(range(drange$x)) + theme_bw()
    } else {
      p <- ggplot(vertebrae.db) + aes_string(input$dpvar, fill = 'SEX') +
        geom_density(alpha = input$alpha, kernel = input$kernel, adjust = input$bandwidth) +
        xlim(range(drange$x)) + theme_bw()
    }
    
  })
  
  output$normTest <- renderPrint({
    st <- shapiro.test(vertebrae.db[,input$dpvar])
    cat(st$method, 'for', input$dpvar, 'is W =', st$statistic, 'with a p-value =', st$p.value)
  })
  
  # Boxplots with ggplot ----
  output$boxPlot <- renderPlotly({
    
    if (input$truedata == FALSE) {
      bp <- ggplot(vertebrae.db, aes_string(x = 'SEX', y = input$bpvar, fill = 'SEX')) +
        geom_boxplot() + guides(fill = FALSE) + theme_bw() +
        stat_summary(fun.y = mean, geom = 'point', shape = 5, size = 2)
      bp + theme(legend.position = 'none')
    } else {
      bp <- ggplot(vertebrae.db, aes_string(x = 'SEX', y = input$bpvar, fill = 'SEX')) +
        geom_boxplot() + guides(fill = FALSE) + geom_jitter(width = 0.1, alpha = 0.5) + 
        stat_summary(fun.y = mean, geom = 'point', shape = 5, size = 2) + theme_bw()
      bp + theme(legend.position = 'none')
      
    }
    
  })
  
  # Scatterplots with ggplot ----
  output$scatterPlot <- renderPlotly({
    
    if (input$filter2 == FALSE) {
      sp <- ggplot(vertebrae.db, aes_string(x = input$xvar, y = input$yvar)) +
        geom_point() + geom_smooth(span = input$spanner) + theme_bw()
    }else{
      sp <- ggplot(vertebrae.db, aes_string(x = input$xvar, y = input$yvar, colour = 'SEX')) +
        geom_point() + geom_smooth(span = input$spanner) + theme_bw()
    }
  })
  
  # Correlation matrix with corrplot ----
  output$corrPlot <- renderPlot({
    col1 <- colorRampPalette(c('#fb8c7f', '#808080', '#00c8ce'))
    if (input$cor.filter == 'males') {
      M <- cor(vertebrae.male[,-c(1:2)], method = input$cor.method, use = 'pairwise.complete.obs')
    } else if (input$cor.filter == 'females') {
      M <- cor(vertebrae.female[,-c(1:2)], method = input$cor.method, use = 'pairwise.complete.obs')
    } else {
      M <- cor(vertebrae.db[,-c(1:2)], method = input$cor.method, use = 'pairwise.complete.obs')
    }

    corrplot.mixed(M, lower = input$lower.visuals, upper = input$upper.visuals,
                   order = if (input$orderVisuals == 'manual') 'original' else input$orderVisuals,
                   tl.pos = 'd', tl.cex = 0.7, tl.col = 'black', number.cex = 0.7,
                   hclust.method = input$plotHclustMethod, 
                   addrect = input$plotHclustAddrect,
                   lower.col = col1(nrow(M)),
                   upper.col = col1(nrow(M))
    )
  })
  
  
  # Estimate Missing Values ----
  observe({
    ex <<- input$variable
    
    # Can also set the label and select items
    updateSelectInput(session, "variable2",
                      label = '(missing vertebrae types - those you want to estimate)',
                      choices = c(outnames[! codenames %in% ex])
    )
  })

  
  data <- reactive({
    if (input$SEX == "Male") {
      vertebrae.male[ , input$variable, drop = FALSE]
    } else if (input$SEX == "Female") {
      vertebrae.female[ , input$variable, drop = FALSE]
    }else{
      vertebrae.db[ , input$variable, drop = FALSE]
    }
  })
  
  antidata <- reactive({
    if (input$SEX == "Male") {
      antivertebrae.db <- vertebrae.male[ , -c(1:3), drop = FALSE]
    } else if (input$SEX == "Female") {
      antivertebrae.db <- vertebrae.female[ , -c(1:3), drop = FALSE]
    }else{
      antivertebrae.db <- vertebrae.db[ , -c(1:3), drop = FALSE]
    }
    remove <- names(antivertebrae.db) %in% input$variable2
    antivertebrae.db[, remove, drop = FALSE]
  })
  
  
  output$vars <- renderUI({
    
    # If missing input, return to avoid error later in function
    if(length(data()) < 1)
      return()
    
    # Get the data set value for variable name
    lapply(colnames(data()), function(i){
      numericInput(
        inputId = paste0('io', i), label = i,
        value = round(mean(data()[[i]], na.rm = T), digits = 1),
        min = round(mean(data()[[i]], na.rm = T) - 6 * sd(data()[[i]], na.rm = T), digits = 1),
        max = round(mean(data()[[i]], na.rm = T) + 6 * sd(data()[[i]], na.rm = T), digits = 1),
        step = 0.5
      )
    })
  })
  
  values <- reactiveValues(df_data = NULL)
  
  observeEvent(input$Go, {
    if(length(data()) == 0 | length(antidata()) == 0) {
      
      output$total <- renderPrint({
        cat('We cannot perform this calculation. Add at least one input or output variable.')
      })
      
      output$predictions <- renderTable({
        error <- as.data.frame('You need to select both input and output variables')
        colnames(error) <- 'ERROR!'
        error
      })
      
    }else if(length(data()) >= 24){
      
      output$total <- renderPrint({
        cat('We cannot perform this calculation. Remove at least one input variable.')
      })
      
      output$predictions <- renderTable({
        error <- as.data.frame('You cannot select all variables! Otherwise, what is there left to estimate?')
        colnames(error) <- 'ERROR!'
        error
      })
      
    }else{
      dif <- as.data.frame(lapply(colnames(data()), function(i) {
        input[[paste0('io', i)]]
      }), col.names = colnames(data()))
      
      # [+] dsnavega: mean_imputation() ----
      mean_imputation <- function(x) {
        x <- sapply(x, function(x) {
          is_na <- is.na(x)
          x[is_na] <- mean(x, na.rm = T)
          x
        })
        x
      }
      
      # [+] X, Input; Y, Output (mean_imputation) ----
      X <- mean_imputation(data())
      Y <- mean_imputation(antidata())
      elfo <- input$elf
      # [+] Train grnnet Object
      object <- grnnet(x = X, y = Y, alpha = elfo)
      
      # metrics <- as.data.frame(object$metrics)
      metrics <- evaluate_grnnet(object)
      pred <- predict(object, dif)
      estimated_vertebrae <- names(pred)
      pred <- matrix(unlist(pred), ncol = 3, byrow = TRUE)
      colnames(pred) <- c('Predicted', 'Lower', 'Upper')
      temp <- data.frame(pred, metrics[,1:7])
      rownames(temp) <- estimated_vertebrae
      
      values$df_data <- temp
      
      
      output$total <- renderPrint({
        sum_in <- sum(as.numeric(dif))
        sum_out <- sum(as.numeric(values$df_data[-1,'PREDICTIONS']))
        sumt <- sum_out + sum_in
        cat('Through the sum of all variables,
									partial total column size is', sumt, 'mm')
      })
      
      output$predictions <- renderTable({
        values$df_data
      }, rownames = TRUE, align = 'r')
    }
  })
  
}
