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
    
    df<- filedata()
    if (is.null(df)) return(NULL) # return nothing if nothing is uploaded
    
    df$temp= df[,2]
    df$times = as.POSIXct(df$Time, format="%Y-%m-%d %H:%M:%S",tz="America/Lima")
    
    
    sums1a = subset.data.frame(df, select = c(times,temp))
    
    
    # a function that identifies the temp peaks, when the stove
    # has been used
    stove_uses <- function(x, t = 25, h = 1){
        a<-x %>% mutate(peak = ifelse(temp > t, 1,0))
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
    b<- stove_uses(sums1a, t = 25, h = 2)
                               
                               
    plot(b$times, b$temp, type = "l", main = "Stove Temperature")
    points(b$times, b$peak3*25, col = "red")
    
    })
    

})

