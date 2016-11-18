library(ggplot2)
library(dplyr)


#change to desired working directory
setwd("C:/Users/Lenovo T440s/Desktop/Data Science II/
      CookStove/CookstoveProject/Raw_data")


hh1a = read.csv("Pilot_HH1_chimneyside.csv")

hh1a$temp= hh1a[,2]

hh1a$times = as.POSIXct(hh1a$Time, format="%Y-%m-%d %H:%M:%S",tz="America/Lima")

sums1a = subset.data.frame(hh1a, select = c(times,temp))

##PLOT
# plot the entire series
plot(sums1a$times, sums1a$temp, main = "Temperature Plot", type = "l")



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




b<- stove_uses(sums1a, h = 2)


# a plot showing the selected peaks above 25 degrees celcius
# Assume that we would start with the stove not being in use!

plot(b$times, b$temp, type = "l")
points(b$times, b$peak3*25, col = "red")


# numer of times the stove has been used in the last week:
sum(b$peak3)




