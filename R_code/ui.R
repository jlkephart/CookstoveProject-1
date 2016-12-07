library(shiny)

shinyUI(fluidPage(
    titlePanel("Stove Temperature Info"),
    
    # drop down menu with time selections
    
    selectInput("time", label = h3("Time Period"),
                choices = list("Week" = 1, "Month" = 2, "Three Months" = 3, "All Time" = 4),
                selected = 1),
    
    #fluidRow(column(3, verbatimTextOutput("value"))),
    
    sidebarLayout(
        
        
        
        sidebarPanel(
            fileInput('datafile', 'Upload CSV file', accept=c('text/csv'))),
        
        # selectInput("select", label = h3("Time Period"),
        #choices = list("Week" = 1, "Month" = 2, "Three Months" = 3, "All Time" = 4),
        # selected = 1),
        
        
        
        mainPanel(
            textOutput("text1"),
            textOutput("text1b"),
            textOutput("text1c"),
            textOutput("text2"),
            plotOutput("plot1"),
            textOutput("text3")
            
        )
    )
))