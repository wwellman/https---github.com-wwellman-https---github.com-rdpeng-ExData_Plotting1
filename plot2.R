plot2 <- function (  ) { 

## Exploratory Data Analysis Course
## Course Project 1
## Plot 2
## Directory "Coursera_Exploratory_Data"
## Power Consumption Data
        
## Line graph is needed
## Y axis - global active power - title Global Active Power (killowatts)
## X axis - day of the week - Th, F, SA
## X axis is sorted by day of week, then by hour, suggesting sort (order) by date, then by hour - title DOW
## hour must be coerced, use strptime() function 
        
## set working directory
        
setwd("C:/Coursera_Exploratory_Data")

## read input file
        
hcpfilename <- "household_power_consumption.txt"
        
dfinput <- read.table(hcpfilename, header = TRUE, sep =";")
        
## Checkpoint 1 - check dftargetcoerce data frame accuracy
sink("dfinput.txt")
print(dfinput)
sink()

## Create data frame with correct data types using coercion

dftargetcoerce <- data.frame( Date = as.Date( dfinput[ ,1], format = "%d/%m/%Y" )
                              , Time = dfinput [, 2 ]
                              , Global_active_power = as.numeric(dfinput[ ,3])
                              , dfinput[, 4:5]
                              , TestDateTime = paste(dfinput [ ,1], dfinput [ ,2], sep = " ")
                              , DateTime = as.numeric(strptime(paste(dfinput [ ,1], dfinput [ ,2]), 
                                  format = "%d/%m/%Y %H:%M:%S" )))



## Checkpoint 2 - check coerce accuracy
sink("dftargetcoerce.txt")
print(dftargetcoerce)
sink()




dftarget <- dftargetcoerce [(dftargetcoerce$Date == "2007-02-01" | dftargetcoerce$Date == "2007-02-02") , ]

## Checkpoint 3 - check segmentation dates of 02/01/2007 and 02/02/2007 
sink("dftarget.txt")
print(dftarget)
sink()


## Create Axis Vector

atvector <- c(1170304300, 1170400000, 1170480000 )
labelsvector <- c("Thu","Fri","Sat")

## write output to .png file

png(filename = "plot2.png", width = 480, height = 480, units = "px", pointsize = 12, bg = "white") ## Open .png device

## Create plot and send to a file, the .png device


with(dftarget, plot(dftarget$DateTime,dftarget$Global_active_power,type="l", ylab = "Global Active Power (kilowatts)", 
                   xlab = " ", xaxt = "n")  )


axis (1, at = atvector,labels = labelsvector)


dev.off()  ## Close the PNG device



plot(dftarget$DateTime,dftarget$Global_active_power,type="l", ylab = "Global Active Power (kilowatts)", 
     xlab = " ", xaxt ="n")

axis (1, at = atvector,labels = labelsvector)

}  ## End of plot2 function

