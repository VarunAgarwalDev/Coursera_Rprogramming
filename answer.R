corr <- function(directory, threshold = 0){
        files_list=list.files(directory,full.names=T)
        source("complete.R")
        for(i in 1:332){
                if(sum(complete.cases(read.csv(files_list[i])))>threshold){
                data <- data.frame(complete.cases(read.csv(files_list[i])))
                }
        }
        #cor(data$nitrate,data$sulfate,"complete.obs")
	data
}
