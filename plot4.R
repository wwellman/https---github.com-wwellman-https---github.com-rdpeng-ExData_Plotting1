plot4 <- function (  ) { 
        
        ## Exploratory Data Analysis Course
        ## Course Project 1
        ## Plot 4
        ## Directory "Coursera_Exploratory_Data"
        ## Power Consumption Data
        
        ## Data Elements
        ## 1. Date;
        ## 2. Time;
        ## 3. Global_active_power;
        ## 4. Global_reactive_power;
        ## 5. Voltage;
        ## 6. Global_intensity;
        ## 7. Sub_metering_1;
        ## 8. Sub_metering_2;
        ## 9. Sub_metering_3
        
        ## Line graph plotting energy sub metering 1,2,3 
        ## Y axis - energy sub metering 1,2,3
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
        
        dfcoerce <- data.frame(dfinput [ , 1:9]
                               , DateFormat = as.Date(dfinput[ ,1], format = "%d/%m/%Y") )
        
        ## Checkpoint 2 - check dfcoerce data frame accuracy
        sink("dfcoerce.txt")
        print(dfcoerce)
        sink()
        
        ## select records for target dates
        
        dftarget <- dfcoerce [(dfcoerce$DateFormat == "2007-02-01" | dfcoerce$DateFormat == "2007-02-02") , ]
        
        ## checkpoint 3 - check target data frame
        sink("dftarget1.txt")
        print(dftarget)
        sink()
        
        
        dftargetcoerce <- data.frame( Date = dftarget[ ,1]
                                      , Time = dftarget [, 2 ]
                                      , Global_active_power = as.numeric(dftarget[ ,3])
                                      , dftarget[, 4:6]
                                      , Sub_metering_1 = as.numeric (dftarget [ ,7])
                                      , Sub_metering_2 = as.numeric (dftarget [ ,8])
                                      , Sub_metering_3 = as.numeric (dftarget [ ,9])
                                      , DateFormat     =             dftarget [ ,10]
                                      , TestDateTime = paste(dftarget [ ,10], dftarget[ ,2], sep = " ")
                                      , DateTime = as.numeric(strptime(paste(dftarget [ ,1], dftarget [ ,2]), 
                                                                       format = "%d/%m/%Y %H:%M:%S" )))
        
        
        
        ## Checkpoint 4 - check coerce accuracy
        sink("dftargetcoerce.txt")
        print(dftargetcoerce)
        sink()
        
        ## create plot in .png file and on console
        
        
        ## Create Axis Vector
        
        atvector <- c(1170304300, 1170400000, 1170480000 )
        labelsvector <- c("Thu","Fri","Sat")
        
        
        
        ## write output to .png file
        
        ## Open .png device
        
        
        
        png(filename = "plot4.png", width = 480, height = 480, units = "px", pointsize = 12, bg = "white") 
        
        ## set parameters to print two graphs on same line
        
        par(mfrow = c(2,2) , mar = c(5, 4, 2, 1))
        
        
        ## create first two plots
        
        ##  plot 1

        with (dftargetcoerce, plot(dftargetcoerce$DateTime, dftargetcoerce$Global_active_power, type = "l"
                              , ylab = "Global Active Power", xlab = " ", xaxt = "n") )
        
        axis (1, at = atvector, labels = labelsvector)
        
        ## plot 2
        
        with (dftargetcoerce, plot(dftargetcoerce$DateTime, dftargetcoerce$Voltage, type = "l"
                                   , ylab = "Voltage", xlab = "datetime", xaxt = "n"))
        
        axis (1, at= atvector, labels = labelsvector)
        
        ##  plot 3
        
        with(dftargetcoerce, plot(dftargetcoerce$DateTime, dftargetcoerce$Sub_metering_1, type = "l"
                                  , ylab = "Energy Sub metering", xlab = " ", xaxt = "n" ))
        
        lines (dftargetcoerce$DateTime, dftargetcoerce$Sub_metering_2, type = "l", col="red")
        
        lines (dftargetcoerce$DateTime, dftargetcoerce$Sub_metering_3, type = "l", col="blue")
        
        axis (1, at = atvector, labels = labelsvector)
        
        legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
               , col = c("black", "red", "blue"), lty = 1, lwd = 2)
        
        ## plot 4
        
        plot(dftargetcoerce$DateTime,dftargetcoerce$Global_reactive_power,type="l", ylab = "Global_reactive_power", 
             xlab = "datetime ", xaxt ="n")
        
        axis (1, at = atvector,labels = labelsvector)

        
        
        
        dev.off()  ## Close the PNG device
        
        
        ## Create plot on console
        
        par(mfrow = c(2,2) , mar = c(5, 4, 2, 1))
        
        
        ## Print first 2 plots on console
        

        plot(dftargetcoerce$DateTime,dftargetcoerce$Global_active_power,type="l", ylab = "Global Active Power (kilowatts)", 
             xlab = " ", xaxt ="n")
        
        axis (1, at = atvector,labels = labelsvector)
        
        
        plot(dftargetcoerce$DateTime,dftargetcoerce$Global_active_power,type="l", ylab = "Global Active Power (kilowatts)", 
             xlab = " ", xaxt ="n")
        
        axis (1, at = atvector,labels = labelsvector)
        
        
        ## print second 2 plots on console
             
        
        
        par(mfrow = c(2,2) , mar = c(5,4,2,1))
        
        
        plot(dftargetcoerce$DateTime,dftargetcoerce$Sub_metering_1,type="l", xaxt="n"
             , ylab = "Energy Sub metering", xlab = " ") 
        
        lines (dftargetcoerce$DateTime, dftargetcoerce$Sub_metering_2, type = "l", col="red")
        
        lines (dftargetcoerce$DateTime, dftargetcoerce$Sub_metering_3, type = "l", col="blue")
        
        axis (1, at = atvector,labels = labelsvector)
        
        legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
               , col = c("black", "red", "blue"), lty = 1, lwd = 2)
        
        plot(dftargetcoerce$DateTime,dftargetcoerce$Global_reactive_power,type="l", ylab = "Global_reactive_power", 
             xlab = "datetime ", xaxt ="n")
        
        axis (1, at = atvector,labels = labelsvector)
        
        
        
}  ## End of plot 4 function