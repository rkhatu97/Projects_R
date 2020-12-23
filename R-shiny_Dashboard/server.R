shinyServer(
  function(input, output, session){
    observe({
      housing <- input$file1
      year_2006 <- as.data.frame(housing[which(housing$YrSold==2006),])
      year_2007 <- as.data.frame(housing[which(housing$YrSold==2007),])
      year_2008 <- as.data.frame(housing[which(housing$YrSold==2008),])
      year_2009 <- as.data.frame(housing[which(housing$YrSold==2009),])
      year_2010 <- as.data.frame(housing[which(housing$YrSold==2010),])
      updateSelectInput(session, "var_1",
                        choices = colnames(housing[,-c(1,78)]),
                        selected = "MSZoning"
      )
    }
      
    )
    output$plot <- renderPlot({
      colm <- input$var_1
      k <- eval(as.name(paste("year",input$sale, sep = "_")))
      c <- table(k[,colm])
      c <- as.data.frame(c)
      ggplot(c, aes(x = Var1, y = Freq)) + 
        geom_bar(fill = input$color, color = "grey30", width = 1, stat = "identity") +
        xlab(input$var_1) +
        ylab("Count") +
        ylim(c(0, max(input$bin))) +
        coord_cartesian(xlim = c(min(input$bin_x), max(input$bin_x))) +
        geom_text(aes(x = Var1, y = Freq, label = Freq), vjust=-1) +
        ggtitle("Housing Data",
                subtitle = input$var_1)
      
      
    })
  })
