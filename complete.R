complete <- function(directory, id = 1:332) {
       ##Gets full name of file and makes a data frame
        files_list <- list.files(directory, full.names=TRUE)
    		
			data <- data.frame()
			sum_data <- data.frame()
			## A for loop to make a data table with the files
	        for(i in id)
	        {
						
	            info <- complete.cases(read.csv(files_list[i]))
				sum_data <- rbind(sum_data, sum(info))
	        }
	        
		
		df <- data.frame(cbind(id, sum_data))
		names(df) <- c("id", "nobs")
		df
		 	   
}
