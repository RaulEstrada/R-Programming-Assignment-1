pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating the
    ## location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating the
    ## name of the pollutant for which we will calculate the mean; 
    ## either "sulfate" or "nitrate"
    
    ## 'id' is an integer vector indicating the monitor ID numbers 
    ## to be used
    
    ## Return the mean of the pollutant across all monitors listed in
    ## the 'id' vector (ignoring NA values)
    ## NOTE: Result not rounded!
    totalSum <- 0
    totalCount <- 0
    for (elemnt in id) {
        formattedId <- sprintf("%03d", elemnt)
        fileName <- paste("./",directory, "/", formattedId, ".csv", sep = "")
        data <- read.csv(fileName)
        target <- data[, c(pollutant)]
        target <- target[!is.na(target)]
        totalSum <- totalSum + sum(target)
        totalCount <- totalCount + length(target)
    }
    totalSum/totalCount;
}