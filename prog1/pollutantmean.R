pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    source(paste(getwd(),"expandDirectory.R",sep="/"))
    directory <- expandDirectory(directory)
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    df <- data.frame()
    for (i in id) {
        filename <- sprintf("%03d.csv", as.numeric(i)) # left pad numbers
        df <- rbind(df, read.csv(paste(directory,filename,sep="/")))
    }
        
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    round(mean(df[[pollutant]], na.rm=TRUE),digits=3)
}