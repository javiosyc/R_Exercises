complete <- function(directory, id = 1:332) {
        if(!length(directory) == 1 ){
                stop('directory.length must be one!')        
        }
        if(!file.exists(directory)) {
                stop('directory does not exist!')        
        }
        df = data.frame()
        for(index in id) {
                fileName <- getFileNameforComplete(directory,index)
                if(file.exists(fileName) ){
                        table <- read.csv(fileName)
                        row <- list(index,getCompleteCasesNum(table))
                        if(length(df)==0){
                                df <- data.frame(row)
                                colnames(df) <- c("id","nobs")
                        }else{
                                df <- rbind(df,row)
                        }
                }
        }
        df
}


getFileNameforComplete <- function (dir,index) {
        fileName <- paste(dir, formatC(index, width=3, flag="0"), sep="/")
        paste( fileName,"csv", sep=".")
}

getCompleteCasesNum <- function (table) {
        bad <- complete.cases(table)
        nrow(table[bad,])
}