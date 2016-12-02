corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating the location 
    ## of the CSV files
    ## 'threshold' is a numeric vector of length 1 indicating the number of 
    ## completely observed observations (on all variables) required to compute
    ## the correlation between nitrate and sulfate; default is 0
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
    
    directoryName <- paste("./", directory, "/", sep = "")
    filenames <- list.files(path = directoryName, pattern = "*.csv", full.names = FALSE)
    result = c()
    for (filename in filenames) {
        fName <- paste(directoryName, filename, sep = "")
        CSVFile <- read.csv(fName)
        totalComplete <- 0
        completeData <- c()
        for (indx in 1:nrow(CSVFile)) {
            sumNAs <- sum(is.na(CSVFile[indx,]))
            if (sumNAs == 0) {
                totalComplete <- totalComplete + 1
                completeData <- rbind(completeData, CSVFile[indx,])
            }
        }
        if (totalComplete >= threshold && totalComplete > 0) {
            corrValue <- cor(completeData$sulfate, completeData$nitrate)
            result <- c(result, corrValue)
        }
    }
    if (length(result) == 0) {
        result <- c(0)
    }
    result
}