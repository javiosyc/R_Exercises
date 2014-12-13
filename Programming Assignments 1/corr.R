corr <- function(directory, threshold = 0) {
        if(!length(directory) == 1 || !length(threshold)==1 ){
                stop('directory.length or threshold  must be one!')        
        }
        if(!file.exists(directory)) {
                stop('directory does not exist!')        
        }
        completeCase <- complete(directory)
        isGoodCase <- completeCase$nobs > threshold
        goodCase <- completeCase[isGoodCase,]
        fileList <- goodCase$id
        
        result <- vector(mode="numeric")
        for(index in fileList) {
                fileName <- getFileNameforComplete(directory,index)
                if(file.exists(fileName) ){
                        data <- read.csv(fileName)
                        result <-append(
                                result,cor(data[,"sulfate"],data[,"nitrate"],use="na.or.complete"))
                }
        }
        result
}