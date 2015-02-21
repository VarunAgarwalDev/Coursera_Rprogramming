pollutantmean <- function(directory, pollutant, id = 1:332)
{
        ##Gets full name of file and makes a data frame
        files_list <- list.files(directory,full.names=TRUE)
        data <- data.frame()
        
	## A for loop to make a data table with the files
        for(i in id)
        {
                data<-rbind(data, read.csv(files_list[i]))
        }
        
	##Determining the mean of the pollutants
        colMeans(data[pollutant], na.rm=TRUE)
        
}
