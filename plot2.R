plot2 <- function(directory, name)
{
  ## 'directory' is a character vector of length 1 indicating the location of data for plotting
  ## 'name' is the file name with extension '*.txt'
  
  
  
  ## Checking if the directory exists
  if(!file.exists(directory))  {  stop(paste("The directory: ",directory, " was not found"))   }
  
  ## Reading data file 
  filename <- paste(directory,name, sep ="/")
  if(file.exists(filename))
  {
    data <- read.table(filename,sep = ";",header = TRUE, skip = 0, na.strings = "?", stringsAsFactors = FALSE)
    data <- na.omit(data)
  }
  else {  stop(paste("The data file: ",filename, " was not found"))   }
  
  ## opening the graphics device
  filename <- paste(directory,"plot2.png", sep ="/")
  png(filename,width = 480, height = 480)
  
  ## producing a plot
  
  data$fixed_Data <- as.POSIXct(paste(data$Date, data$Time), format = "%d/%m/%Y %T")
  sub.data <- subset(data, as.Date(fixed_Data) >= '2007-02-01' & as.Date(fixed_Data) <= '2007-02-02' )
  
  plot(sub.data$fixed_Data, sub.data$Global_active_power, type = "l",main = "", 
       ylab = "Global Active Power (kilowatts)", xlab = "", axes = TRUE)
  
  ## closing graphics device
  dev.off()  
}