library(shiny)
library(shinyWidgets)
library(ggplot2)
library(shinydashboard)
housing <- read.csv("House_Price_data.csv")
year_2006 <- as.data.frame(housing[which(housing$YrSold==2006),])
year_2007 <- as.data.frame(housing[which(housing$YrSold==2007),])
year_2008 <- as.data.frame(housing[which(housing$YrSold==2008),])
year_2009 <- as.data.frame(housing[which(housing$YrSold==2009),])
year_2010 <- as.data.frame(housing[which(housing$YrSold==2010),])

ui <- fluidPage(
  dashboardHeader(title = span("Housing Data Analysis", 
                               style = "color: white; font-size: 40px")),
  
  setBackgroundColor(color = "grey",
                     gradient = c("linear","radial"),
                     shinydashboard = FALSE),
  sidebarLayout(
    sidebarPanel(
      tags$style(".well {background-color:[black];}"),
      selectInput(
        inputId = "sale",
        label = "Year Sold",
        c("2006", "2007", "2008", "2009", "2010")
      ),
      selectInput(
        inputId = "var_1", 
        label = "Housing Data per Year", 
        choices = colnames(housing[,-c(1,78)]), selected = "MSZoning"),
      
      radioButtons(
        inputId = "color", 
        label = "Color Of Plot", 
        list("Dark Grey", "Black", "Steel Blue")),
      
      sliderInput(
        inputId = "bin", 
        label = "Y limit", min = 0, 
        max = 1000000,
        value = 15, 
        step = 5),
      
      sliderInput(
        inputId = "bin_x", 
        label = "X limit", min = 1, 
        max = 1000,
        value = c(1,2), 
        step = 1)
      
    ),
    mainPanel(
      
      plotOutput("plot")
    )
    
  )
)

server <- function(input, output){
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
  }
shinyApp(ui = ui, server = server)
