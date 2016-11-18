library(shiny)


################ This shiny App just plots whatever .csv file you give it. 
shinyServer(function(input, output){
    

    # read in the .csv file of interest
    filedata <- reactive({
        infile <- input$datafile
        if (is.null(infile)) {
            # User has not uploaded a file yet
            return(NULL)
        }
        read.csv(infile$datapath)
    })
    

##Do the necessary data management, and plot
    
output$plot1<- renderPlot({   
    
    #enter start and end date for SUMS 1A ***use "yyyy-mm-dd" format***
    S1A_strtdat <- "2016-08-10"
    S1A_enddat <- "2016-08-12"
    
    #enter start and end time for SUMS 1A ***use "HH:MM:SS" format
    S1A_strttim <- "06:30:00"
    S1A_endtim <- "07:15:00"
    
    df<- filedata()
    if (is.null(df)) return(NULL) # return nothing if nothing is uploaded
    
    df$temp= df[,2]
    df$times = as.POSIXct(df$Time, format="%Y-%m-%d %H:%M:%S",tz="America/Lima")
    
    
    #designate start time
    strtim1a = as.POSIXct(paste(S1A_strtdat, S1A_strttim, sep = " "),
                          format="%Y-%m-%d %H:%M:%S",tz="America/Lima")
    
    #designate end time
    endtim1a = as.POSIXct(paste(S1A_enddat, S1A_endtim, sep = " "),
                          format="%Y-%m-%d %H:%M:%S",tz="America/Lima")
    
    sums1a = subset.data.frame(df, 
                               df$times > strtim1a & df$times < endtim1a,
                               select = c(times,temp))
    

        
        plot(sums1a$times, sums1a$temp, main = "Temperature Plot", type = "l")
    
    })
    

})

