library(ggplot2)

#change to desired working directory
setwd("C:/Users/Lenovo T440s/Desktop/Data Science II/
      CookStove/CookstoveProject/Raw_data")

##SUMS 1A
#enter file name
S1A_file = "Pilot_HH1_chimneyside.csv"

#enter start and end date for SUMS 1A ***use "yyyy-mm-dd" format***
S1A_strtdat = "2016-08-10"
S1A_enddat = "2016-08-12"

#enter start and end time for SUMS 1A ***use "HH:MM:SS" format
S1A_strttim = "06:30:00"
S1A_endtim = "07:15:00"

#enter graph title
title="Temperature over pilot period"

##Process data 

hh1a = read.csv(S1A_file)
head(hh1a)
hh1a$temp= hh1a[,2]

hh1a$times = as.POSIXct(hh1a$Time, format="%Y-%m-%d %H:%M:%S",tz="America/Lima")
head(hh1a)

#designate start time
strtim1a = as.POSIXct(paste(S1A_strtdat, S1A_strttim, sep = " "),
                      format="%Y-%m-%d %H:%M:%S",tz="America/Lima")

#designate end time
endtim1a = as.POSIXct(paste(S1A_enddat, S1A_endtim, sep = " "),
                      format="%Y-%m-%d %H:%M:%S",tz="America/Lima")

sums1a = subset.data.frame(hh1a, 
                           hh1a$times > strtim1a & hh1a$times < endtim1a,
                           select = c(times,temp))
head(sums1a)

##PLOT

date_format_tz = function(format = "%m/%d %H:%M", tz = "America/Lima"){
    function(x) format(x, format, tz=tz)
}

base <- ggplot() + 
    geom_line(data = sums1a, aes(times,temp, color = "inside chimney")) +
    #  geom_line(data = sums1b, aes(times,temp, color = "in cage")) +
    ylab("Temp") + labs(color="SUMS location") 

base + scale_x_datetime(name = "Time", date_breaks="8 hours", date_minor_breaks = "2 hours",
                        labels = date_format_tz("%m/%d %H:%M", tz="America/Lima")) + 
    ggtitle(title)

