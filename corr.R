corr <- function(directory, threshold = 0){
	
	files_list = list.files(directory, full.names = TRUE)
	source("complete.R")
	DF.complete <- complete(directory, 1:332)
	
	##The data for the air quality
	data <- read.csv(files_list[i])
	
	target_id <- data.frame()
	final <- data.frame()
	
	##Get target id monitors
	for(i in 1:332){
		if(DF.complete$nobs[i] > threshold){
			target_id <- rbind(target_id, DF.complete$id[i])
		}
	}
	names(target_id) <- c("id")
	
	##Creates a vector of the pollutants from the target ids
	for(y in 1:nrow(target_id)){
		id_monitor <- target_id$id[y]
		list_pollutants <- read.csv(files_list[id_monitor])
		final <- rbind(final, list_pollutants)
	}
	correlation <- vector()
	na_rm <- na.omit(final)
	for(m in nrow(final)){
		correlation <- c(correlation, cor(na_rm$sulfate, na_rm$nitrate))
	}
	correlation

}
