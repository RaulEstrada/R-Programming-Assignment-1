complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating the location
    ## of the CSV files
    ## 'id' is an integer vector indicating the monitor ID numbers to be used
    ## Return a data frame of the form:
    ## id   nobs
    ## 1    117
    ## 2    1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the number of 
    ## complete cases
    df <- c()
    for (el in id) {
        formattedId <- sprintf("%03d", el)
        fileName <- paste("./",directory, "/", formattedId, ".csv", sep = "")
        data <- read.csv(fileName)
        totalCount <- 0
        for (indx in 1:nrow(data)) {
            rowNAs <- sum(is.na(data[indx,]))
            if (rowNAs == 0) {
                totalCount <- totalCount +1
            }
        }
        if (totalCount != 0) {
            df <- rbind(df, c(id = el, nobs = totalCount))
        }
    }
    as.data.frame(df)
}