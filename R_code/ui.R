library(shiny)

shinyUI(pageWithSidebar(
    
    titlePanel("Stove Temperature Info"),
    
    #Selector for file upload
    fileInput('datafile', 'Upload CSV file',
              accept=c('text/csv')),
    
        mainPanel(
            plotOutput("plot1")
        )
))