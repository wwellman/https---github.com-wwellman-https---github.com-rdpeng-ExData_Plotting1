plot1 <- function(   ) {

## Exploratory Data Analysis Course
## Course Project 1
## Plot 1
## Directory "Coursera_Exploratory_Data"
## Power Consumption Data
        
## set working directory
        
setwd("C:/Coursera_Exploratory_Data")

## input file

hcpfilename <- "household_power_consumption.txt"
        
dfinput <- read.table(hcpfilename, header = TRUE, sep =";")

## Checkpoint 1 - check dftargetcoerce data frame accuracy
sink("dfinput.txt")
print(dfinput)
sink()

dftargetcoerce <- data.frame( Date = as.Date( dfinput[ ,1], format = "%d/%m/%Y" ),dfinput [, 2 ], 
                       Global_active_power = as.numeric(dfinput[ ,3]), dfinput[, 4:5] )


## Checkpoint 2 - check dftargetcoerce data frame accuracy
sink("dftargetcoerce.txt")
print(dftargetcoerce)
sink()

dftarget <- dftargetcoerce [(dftargetcoerce$Date == "2007-02-01" | dftargetcoerce$Date == "2007-02-02") , ]

## Checkpoint 3 - check segmentation dates of 02/01/2007 and 02/02/2007 
sink("dftarget.txt")
print(dftarget)
sink()


## write output to .png file

png(filename = "plot1.png", width = 480, height = 480, units = "px", pointsize = 12, bg = "white") ## Open .png device

## Create plot and send to a file, the .png device

with(dftarget, hist(dftarget$Global_active_power, col = "red",main="Global Active Power",
                   xlab = "Global Active Power (kilowatts)"))

dev.off()  ## Close the PNG device




hist(dftarget$Global_active_power, col = "red",main="Global Active Power",xlab = "Global Active Power (kilowatts)")




        
        
}   ## End of Function plot1