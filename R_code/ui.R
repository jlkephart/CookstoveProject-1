library(shiny)

shinyUI(fluidPage(
    
    titlePanel("Stove Temperature Info"),
    
        mainPanel(
            plotOutput("plot1")
        )
))