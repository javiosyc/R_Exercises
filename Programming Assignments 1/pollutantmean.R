pollutantmean <- function(directory, pollutant, id = 1:332) {
        
        if(pollutant !="nitrate" && pollutant !="sulfate") {
                stop('pollutant must be nitrate and sulfate!')
        }
        
        if(! length(directory) ==1 || ! length(directory) ==1) {
                stop('directory.length or pollutant.length must be one!')        
        }
        
        if(!file.exists(directory)) {
                stop('directory does not exist!')        
        }
        
        rowSum <- 0
        sum <- 0
        fileList <- getFileName(directory,id)
        for(fileName in fileList) {
                if(file.exists(fileName)) {
                        table <- read.csv (fileName)
                        data <- getData(table,pollutant)
                        rowSum = rowSum + length(data)
                        sum = sum + sum(data)
                }
        }
        sum/rowSum
}

getFileName <- function (dir,index) {
        fileName <- paste(dir, formatC(index, width=3, flag="0"), sep="/")
        paste( fileName,"csv", sep=".")
}

getData <- function (table,pollutant) {
        data <- table[pollutant]
        bad <- is.na(data)
        data[!bad]
}