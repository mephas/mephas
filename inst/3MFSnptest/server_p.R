#****************************************************************************************************************************************************3.npp

names3 <- reactive({
  x <- unlist(strsplit(input$cn3, "[\n]"))
  return(x[1:3])
  })

  C <- reactive({
    inFile <- input$file3
    if (is.null(inFile)) {
    X <- as.numeric(unlist(strsplit(input$y1, "[,;\n\t]")))
    Y <- as.numeric(unlist(strsplit(input$y2, "[,;\n\t]")))
    validate( need(sum(!is.na(X))>1, "Please input enough valid numeric data") )
    validate( need(sum(!is.na(Y))>1, "Please input enough valid numeric data") )
    validate( need(length(X)==length(Y), "Please make sure two groups have equal length") )
    d <- round(X-Y,4)
    x <- data.frame(X =X, Y = Y, diff = d)
    colnames(x) = names3()
  }

    else {
      if(!input$col3){
    csv <- read.csv(inFile$datapath, header = input$header3, sep = input$sep3)
    }
    else{
    csv <- read.csv(inFile$datapath, header = input$header3, sep = input$sep3, row.names=1)
    }
    validate( need(ncol(csv)>0, "Please check your data (nrow>2, ncol=1), valid row names, column names, and spectators") )
    validate( need(nrow(csv)>1, "Please check your data (nrow>2, ncol=1), valid row names, column names, and spectators") )

    x <- csv[,1:2]
    x$diff <- round(x[, 2] - x[, 1], 4)
    if(input$header3==FALSE){
      colnames(x) = names3()
      }
    }
    return(as.data.frame(x))
    })

  #table
output$table3 <-DT::renderDT(C() ,
    extensions = list(
      'Buttons'=NULL,
      'Scroller'=NULL),
    options = list(
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel'),
      deferRender = TRUE,
      scrollY = 300,
      scroller = TRUE))

  C.des <- reactive({
    x<- C()
    res <- as.data.frame(t(psych::describe(x))[-c(1,6,7), ])
    colnames(res) = names(x)
    rownames(res) <- c("Total Number of Valid Values", "Mean" ,"SD", "Median", "Minimum", "Maximum", "Range","Skew","Kurtosis","SE")
    return(res)
  })

  output$bas3 <- DT::renderDT({  ## don't use renerPrint to do DT::renderDT
    res <- C.des()},

    extensions = 'Buttons',
    options = list(
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel'),
    scrollX = TRUE))


  output$bp3 = plotly::renderPlotly({
    x <- C()
    var <- names(C())[3]
    p<-plot_box1(x, var)
    plotly::ggplotly(p)
    #ggplot(x, aes(x = 0, y = x[,3])) + geom_boxplot(width = 0.2, outlier.color = "red") + xlim(-1,1)+
    #ylab("") + ggtitle("") + theme_minimal()
    })

  # output$info3 <- renderText({
  #   xy_str = function(e) {
  #     if(is.null(e)) return("NULL\n")
  #     paste0("Click to get the value: ", round(e$y, 4))
  #   }
  #   paste0("Y-axis position", "\n", xy_str(input$plot_click3))})

  output$makeplot3 <- plotly::renderPlotly({
    x <- C()
    var <- colnames(x)[3]
    p <- plot_hist1(x, var, input$bin3)
    plotly::ggplotly(p)
    #ggplot(x, aes(x=x[,3])) + geom_histogram(colour="black", fill = "grey", binwidth=input$bin3, alpha=.5, position="identity") + ylab("Frequncy") + xlab("") +  ggtitle("") + theme_minimal() + theme(legend.title=element_blank())
    })
  output$makeplot3.1 <- plotly::renderPlotly({
    x <- C()
    var <- names(x)[3]
    p <- plot_density1(x, var)
    plotly::ggplotly(p)
    #ggplot(x, aes(x=x[,3])) + geom_density() + ggtitle("") + theme_minimal() + ylab("Density") + xlab("") + theme(legend.title=element_blank())
    })

psr.test <- reactive({
  x <- C()
  if (input$alt.md3 =="a"){
    res <- wilcox.test(x[,1], x[,2], paired = TRUE,
      alternative = input$alt.wsr3, exact=NULL, correct=TRUE, conf.int = TRUE)
  }
    if (input$alt.md3 =="b") {
    res <- wilcox.test(x[,1], x[,2], paired = TRUE,
      alternative = input$alt.wsr3, exact=NULL, correct=FALSE, conf.int = TRUE)
  }
  if (input$alt.md3 =="c")  {
    res <- exactRankTests::wilcox.exact(x[,1], x[,2],  paired = TRUE,
      alternative = input$alt.wsr3, exact=TRUE, conf.int = TRUE)

  }
    res.table <- t(data.frame(W = res$statistic,
                              P = res$p.value,
                              EM = res$estimate,
                              CI = paste0("(",round(res$conf.int[1], digits = 4),", ",round(res$conf.int[2], digits = 4), ")")))
    colnames(res.table) <- res$method
    rownames(res.table) <- c("W Statistic", "P Value","Estimated Median","95% Confidence Interval")

    return(res.table)
    })

  output$psr.test.t <- DT::renderDT({
    psr.test()},

    extensions = 'Buttons',
    options = list(
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel'),
    scrollX = TRUE))
