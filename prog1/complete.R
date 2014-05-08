complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    source(paste(getwd(),"expandDirectory.R",sep="/"))
    directory <- expandDirectory(directory)
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of com?dplete cases
    df <- data.frame()
    
    for (i in id) {
        filename <- sprintf("%03d.csv", as.numeric(i)) # left pad numbers
        dfin <- read.csv(paste(directory,filename,sep="/"))
        df <- rbind(df, c(i, sum(complete.cases(dfin))))
    }
    colnames(df) <- c("id", "nobs")
    df  
}