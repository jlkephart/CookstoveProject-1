library(shiny)

    
#change to desired working directory

setwd("C:/Users/Lenovo T440s/Desktop/Data Science II/CookStove/CookstoveProject/Raw_data")
    

################# Input The Data ###################
    S1A_file = "Pilot_HH1_chimneyside.csv"
    
    #enter start and end date for SUMS 1A ***use "yyyy-mm-dd" format***
    S1A_strtdat = "2016-08-10"
    S1A_enddat = "2016-08-12"
    
    #enter start and end time for SUMS 1A ***use "HH:MM:SS" format
    S1A_strttim = "06:30:00"
    S1A_endtim = "07:15:00"
    
    #enter graph title
    title="Temperature over pilot period"

    
    hh1a = read.csv(S1A_file )

    hh1a$temp= hh1a[,2]
    hh1a$times = as.POSIXct(hh1a$Time, format="%Y-%m-%d %H:%M:%S",tz="America/Lima")


    #designate start time
    strtim1a = as.POSIXct(paste(S1A_strtdat, S1A_strttim, sep = " "),
                      format="%Y-%m-%d %H:%M:%S",tz="America/Lima")

    #designate end time
    endtim1a = as.POSIXct(paste(S1A_enddat, S1A_endtim, sep = " "),
                      format="%Y-%m-%d %H:%M:%S",tz="America/Lima")

    sums1a = subset.data.frame(hh1a, 
                           hh1a$times > strtim1a & hh1a$times < endtim1a,
                           select = c(times,temp))


################ Add the Data Plot to the Shiny App #####################
shinyServer(function(input, output){
    ##PLOT
    
    output$plot1<- renderPlot({
        
        plot(sums1a$times, sums1a$temp, main = "Temperature Plot", type = "l")
    
    })
    

})

