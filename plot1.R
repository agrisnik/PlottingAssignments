plot1 <- function(directory, name)
{
    ## 'directory' is a character vector of length 1 indicating the location of data for plotting
    ## 'name' is the file name with extension '*.txt'
    
  
  
    ## Checking if the directory exists
    if(!file.exists(directory))  {  stop(paste("The directory: ",directory, " was not found"))   }
  
    ## Reading data file 
    filename <- paste(directory,name, sep ="/")
    if(file.exists(filename))
    {
      data <- read.table(filename,sep = ";",header = TRUE,skip = 0, na.strings = "?", stringsAsFactors = FALSE)
    }
    else {  stop(paste("The activity label file: ",activitylabels.filename, " was not found"))   }
    
    ## opening the graphics device
    filename <- paste(directory,"plot1.png", sep ="/")
    png(filename,width = 480, height = 480)
    
    
    #producing histogram
    hist(as.numeric(data$Global_active_power), col = "red", main = "Global Active Power", 
         xlab = "Global Active Power (kilowatts)", breaks = 16,axes = TRUE, freq = TRUE)
    
    #closing graphics device
    dev.off()

}