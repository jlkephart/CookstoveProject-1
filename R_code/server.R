library(shiny)
library(dplyr)
library(lubridate)


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
 

# check if the last time is within the last hour or so, if not, give a warning message.
output$text1 <- renderText({     
    df<- filedata()
    if (is.null(df)) return(NULL) # return nothing if nothing is uploaded
    df$temp= df[,2]
    
    tformats <- c("%m/%d%y %H:%M:%S","%m/%d%y %H:%M","%Y-%m-%d %H:%M:%S") #known date formats in raw data
    df$times = parse_date_time(df$Time, tformats, truncated = 1, tz="America/Lima")
    

    sums1a = subset.data.frame(df, select = c(times,temp))
    
    # compare the system time to the last time in the data
    diff <- as.numeric(difftime(Sys.time(), tail(sums1a$times, 1),  units = "mins"))
    # check for missing values
    sums1a <- sums1a %>%
        mutate(temp = ifelse(temp == "blank", "NA", temp))
    nas<-sum(is.na(sums1a$temp))
    # check for extremely low values
    min_temp <- sum(sums1a$temp< -20, na.rm = TRUE)
    
    if(diff > 60 | nas>0 | min_temp>0){
        "Error! Logger stopped functioning more than an hour ago"
    #}else if (nas>0){
        #"There are missing values in the data"
   }

})

# check to see if the logger reaches a sum above 85 degrees
output$text2 <- renderText({ 
    df<- filedata()
    if (is.null(df)) return(NULL) # return nothing if nothing is uploaded
    df$temp= df[,2]
    tformats <- c("%m/%d%y %H:%M:%S","%m/%d%y %H:%M","%Y-%m-%d %H:%M:%S") #known date formats in raw data
    df$times = parse_date_time(df$Time, tformats, truncated = 1, tz="America/Lima")
    sums1a = subset.data.frame(df, select = c(times,temp))
    
    max_temp<- sum(as.numeric(sums1a$temp)>85, na.rm = TRUE)
    if (max_temp > 0){
        "The logger is too close!"
    }
})
    
    

##Do the necessary data management, and plot
    
output$plot1<- renderPlot({   
    
    df<- filedata()
    if (is.null(df)) return(NULL) # return nothing if nothing is uploaded
    
    df$temp= df[,2]
    tformats <- c("%m/%d%y %H:%M:%S","%m/%d%y %H:%M","%Y-%m-%d %H:%M:%S") #known date formats in raw data
    df$times = parse_date_time(df$Time, tformats, truncated = 1, tz="America/Lima")
    
    
    sums1a = subset.data.frame(df, select = c(times,temp))
    sums1a <- na.omit(sums1a)
    
    # a function that identifies the temp peaks, when the stove
    # has been used
    stove_uses <- function(x, t = 25, h = 1){
        a<-x %>% mutate(peak = ifelse(as.numeric(temp) > t, 1,0))
        a$peak2 <- 0
        a$peak3 <- 0
        for (i in 1:(length(a$peak)-1)){
            if(a$peak[i] < a$peak[i+1]){
                a$peak2[i] = max(a$peak2) + 1
            }else{
                a$peak2[i] = 0
            }
        }
        
        
        # look at established peaks one by one. See if they differ by more than
        # h hours. If they do, the peaks are kept, if they don't, the peaks
        # are deleted
        for (i in 1:(max(a$peak2)-1)){
            if (as.numeric(difftime(a$times[a$peak2 == i], a$times[a$peak2 == i+1],
                                    units = "hours")) < -h){
               
                
                 a$peak3[a$peak2 == i] = 1
                
            }else{
                a$peak3[a$peak2 == i] = 0
            }
        }
    # add the final peak to the dataset
        a$peak3[a$peak2 == max(a$peak2)] = 1 
        a
    }
    
# output to plot
    b<- stove_uses(sums1a, t = 24, h = 3 )
                               
                               
    plot(b$times, b$temp, type = "l", main = "Stove Temperature")
    points(b$times, b$peak3*24, col = "red")
    
    })

})




