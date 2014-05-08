corr <- function(directory, threshold = 0, genvector = TRUE) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    source(paste(getwd(),"expandDirectory.R",sep="/"))
    directory <- expandDirectory(directory)
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    v <- vector(mode="numeric") # returnable vector
    id <- 1:332 # all files
    
    if(genvector) {
        for (i in id) {
            filename <- sprintf("%03d.csv", as.numeric(i)) # left pad numbers
            dfin <- read.csv(paste(directory,filename,sep="/"))
            if (sum(complete.cases(dfin)) >  threshold) {
            # run the correlation function, append output on vector
            v <- c(v, round(cor(dfin[complete.cases(dfin),]$nitrate, 
                              dfin[complete.cases(dfin),]$sulfate),
                            digits=4))    
    #            v <- c(v, round(cor(dfin$nitrate,dfin$sulfate),digits=5))
            }
        }
         v
    } else {  
        df <- data.frame()
    
        for (i in id) {
            filename <- sprintf("%03d.csv", as.numeric(i)) # left pad numbers
            dfin <- read.csv(paste(directory,filename,sep="/"))
            if (sum(complete.cases(dfin)) >  threshold) {
                df <- rbind(df, dfin[complete.cases(dfin),])
            }
        }
   
        df      
    }
    # avgs <- ddply(cr,"ID", function(x) cor(x$nitrate, x$sulfate))
    #> summary(avgs)
    #ID              V1          
    #Min.   :  1.0   Min.   :-1.00000  
    #1st Qu.: 81.5   1st Qu.:-0.05282  
    #Median :162.0   Median : 0.10718  
    #Mean   :163.2   Mean   : 0.13684  
    #3rd Qu.:242.5   3rd Qu.: 0.27831  
    #Max.   :332.0   Max.   : 1.00000  
    
    
    ## Return a numeric vector of correlations
}