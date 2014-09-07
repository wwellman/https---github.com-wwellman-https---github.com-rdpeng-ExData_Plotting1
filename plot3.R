plot3 <- function (  ) { 
        
        ## Exploratory Data Analysis Course
        ## Course Project 1
        ## Plot 3
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

        dfcoerce <- data.frame(Date =as.Date( dfinput[ ,1], format = "%d/%m/%Y")
                               , dfinput [ , 2:9] )
        
        ## Checkpoint 2 - check dfcoerce data frame accuracy
        sink("dfcoerce.txt")
        print(dfcoerce)
        sink()
        
        ## select records for target dates
        
        dftarget <- dfcoerce [(dfcoerce$Date == "2007-02-01" | dfcoerce$Date == "2007-02-02") , ]
        
        ## checkpoint 3 - check target data frame
        sink("dftarget1.txt")
        print(dftarget)
        sink()
        
        
        dftargetcoerce <- data.frame( Date = as.Date( dfinput[ ,1], format = "%d/%m/%Y" )
                                      , Time = dfinput [, 2 ]
                                      , Global_active_power = as.numeric(dfinput[ ,3])
                                      , dfinput[, 4:6]
                                      , Sub_metering_1 = as.numeric (dfinput [ ,7])
                                      , Sub_metering_2 = as.numeric (dfinput [ ,8])
                                      , Sub_metering_3 = as.numeric (dfinput [ ,9])
                                      , TestDateTime = paste(dfinput [ ,1], dfinput [ ,2], sep = " ")
                                      , DateTime = as.numeric(strptime(paste(dfinput [ ,1], dfinput [ ,2]), 
                                                                       format = "%d/%m/%Y %H:%M:%S" )))
        
        
        
        ## Checkpoint 4 - check coerce accuracy
        sink("dftargetcoerce.txt")
        print(dftargetcoerce)
        sink()
        
        
        ## Select records for target dates 
        
        dftarget <- dftargetcoerce [(dftargetcoerce$Date == "2007-02-01" | dftargetcoerce$Date == "2007-02-02") , ]
        
        ## Checkpoint 5 - check segmentation dates of 02/01/2007 and 02/02/2007 
        sink("dftarget2.txt")
        print(dftarget)
        sink()
        
        
        ## set label for x axis
        
        xlabel <- c("2/1 A.M.", "2/1 P.M.", "2/2 A.M.", "2/2 P.M.")
        
        ## write output to .png file

        ## png(filename = "plot3.png", width = 480, height = 480, units = "px", pointsize = 12, bg = "white") ## Open .png device
        
        ## Create plot and send to a file, the .png device
        
        
        ## with(dftarget, plot(dftarget$DateTime,dftarget$Sub_metering1,type="l", ylab = "Energy sub metering", 
         ##                   xlab = "Date / Time")  )
        ## lines(dftarget$DateTime, dftarget$Sub_metering2, type = "l")

               
        
        
        ## dev.off()  ## Close the PNG device
        
        ## plot(dftarget$DateTime,dftarget$Sub_metering_1,type="l", ylab = "Energy Sub metering", 
        ##     xlab = "Date / Time")
     
        
}  ## End of plot3 function
