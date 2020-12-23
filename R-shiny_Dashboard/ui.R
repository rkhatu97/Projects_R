library(shiny)
library(shinyWidgets)
library(ggplot2)
library(shinydashboard)

shinyUI(fluidPage(
  dashboardHeader(title = span("Housing Data", 
                               style = "color: white; font-size: 40px")),
  
  setBackgroundColor(color = "grey",
                     gradient = c("linear","radial"),
                     shinydashboard = FALSE),
  sidebarLayout(
    sidebarPanel(
      tags$style(".well {background-color:[black];}"),
      fileInput("file1", "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      selectInput(
        inputId = "sale",
        label = "Year Sold",
        c("2006", "2007", "2008", "2009", "2010")
      ),
      
      selectInput(
        inputId = "var_1", 
        label = "Housing Data per Year", 
        choices = "MSZoning", selected = "MSZoning"),
      
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
)
