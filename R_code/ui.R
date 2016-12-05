library(shiny)


#shinyUI(pageWithSidebar(
    
   # titlePanel("Stove Temperature Info"),
    
    #Selector for file upload
    #fileInput('datafile', 'Upload CSV file',
             # accept=c('text/csv')),
    
       # mainPanel(
            #textOutput("text1")
            #plotOutput("plot1")
        #)
#))


shinyUI(fluidPage(
    titlePanel("Stove Temperature Info"),
    
    sidebarLayout(
        sidebarPanel(fileInput('datafile', 'Upload CSV file',
                                   accept=c('text/csv'))),
        mainPanel(
            textOutput("text1"),
            textOutput("text2"),
            plotOutput("plot1")
            )
    )
))