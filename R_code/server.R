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
        
        
        # we should differentiate the errors, or just have a single error message. 
        if(diff > 60){
            "Error! Logger stopped functioning more than an hour ago"
            
        }
        
    })
    
    
    output$text1b <- renderText({     
        
        df<- filedata()
        if (is.null(df)) return(NULL) # return nothing if nothing is uploaded
        df$temp= df[,2]
        
        tformats <- c("%m/%d%y %H:%M:%S","%m/%d%y %H:%M","%Y-%m-%d %H:%M:%S") #known date formats in raw data
        df$times = parse_date_time(df$Time, tformats, truncated = 1, tz="America/Lima")
        
        
        sums1a = subset.data.frame(df, select = c(times,temp))
        
        # check for missing values
        sums1a <- sums1a %>%
            mutate(temp = ifelse(temp == "blank", "NA", temp))
        nas<-sum(is.na(sums1a$temp))
        
        if(nas > 0){
            "Error! There are missing values"
        }
        
        
    })
    
    
    output$text1c <- renderText({     
        
        df<- filedata()
        if (is.null(df)) return(NULL) # return nothing if nothing is uploaded
        df$temp= df[,2]
        
        tformats <- c("%m/%d%y %H:%M:%S","%m/%d%y %H:%M","%Y-%m-%d %H:%M:%S") #known date formats in raw data
        df$times = parse_date_time(df$Time, tformats, truncated = 1, tz="America/Lima")
        
        
        sums1a = subset.data.frame(df, select = c(times,temp))
        
        min_temp <- sum(sums1a$temp< -30, na.rm = TRUE)
        
        
        if(min_temp > 0){
            "Error! The temperature fell below 30 degees Celcius"
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
        stove_uses <- function(x, tem = 25, h = 1){
            a<-x %>% mutate(peak = ifelse(as.numeric(temp) > tem, 1,0))
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
            #peaknum
        }
        
        
        # Use the stove_uses function and make the plots
        b<- stove_uses(sums1a, tem = 30, h = 3 )
        
        
        if(input$time == 1){
            
            begin<- tail(b$times, 1) - as.difftime(1, unit = "weeks")
            c<- b %>% filter(times > begin)
            plot(c$times, c$temp, type = "l", main = "Stove Temp Within the Last Week")
            # the red peak marks that aren't used to mark peaks sit at zero. We can either ignore
            # that or delete them bu turning them into NA. Method below just called them NA, doenst work.
            #c$peak3[c$peak3 == 0]<-"NA"
            
            points(c$times, c$peak3*30, col = "red")
            
            output$text3 <- renderText({
                paste("The stove was used ", sum(c$peak3, na.rm = T), " times in the last week. These
                      are marked with a red dot on the plot.")
            })
            
            }
        
        
        if(input$time == 2){
            
            begin<- tail(b$times, 1) - as.difftime(30, unit = "days")
            c<- b %>% filter(times > begin)
            plot(c$times, c$temp, type = "l", main = "Stove Temp Within the Last Month")
            points(c$times, c$peak3*30, col = "red")
            # prints out how many times the stove was used in the last period of time
            output$text3 <- renderText({
                paste("The stove was used ", sum(c$peak3, na.rm = T), " times in the last month. These
                      are marked with a red dot on the plot.")
            })
            
            }
        
        if(input$time == 3){
            
            begin<- tail(b$times, 1) - as.difftime(90, unit = "days")
            c<- b %>% filter(times > begin)
            plot(c$times, c$temp, type = "l", main = "Stove Temp in the Last 3 Months")
            points(c$times, c$peak3*30, col = "red")
            
            output$text3 <- renderText({
                paste("The stove was used ", sum(c$peak3, na.rm = T), " times in the last three months. These
                      are marked with a red dot on the plot.")
            })
            
            }
        
        if(input$time == 4){
            
            plot(b$times, b$temp, type = "l", main = "Stove Temp Over All Time")
            points(b$times, b$peak3*30, col = "red")
            
            output$text3 <- renderText({
                paste("The stove was used ", sum(b$peak3, na.rm = T), " times during the whole period. These
                      are marked with a red dot on the plot.")
            })
            
            }
        
        
        
    })
    
    
        })




