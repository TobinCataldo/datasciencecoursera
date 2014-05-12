## rankhospital that takes three arguments: the 2-character abbreviated name 
## of a state (state), an outcome (outcome), and the ranking of a hospital in 
## that state for that outcome (num).
## rankhospital("MD", "heart failure", 5) would return a character vector 
## containing the name of the hospital with the 5th lowest 30-day death rate
## for heart failure. The num argument can take values "best", "worst", or an 
## integer indicating the ranking (smaller numbers are better). If the number 
## given by num is larger than the number of hospitals in that
## state, then the function should return NA.

## > source("rankhospital.R")
## > rankhospital("TX", "heart failure", 4)
## [1] "DETAR HOSPITAL NAVARRO"
## > rankhospital("MD", "heart attack", "worst")
## [1] "HARFORD MEMORIAL HOSPITAL"
## > rankhospital("MN", "heart attack", 5000)
## [1] NA

rankhospital <- function(state, outcome, num="best") {
    ## Read outcome data
    ## Check that state, outcome and num are valid
    
    source(paste(getwd(),"expandDirectory.R",sep="/"))
    directory <- expandDirectory("pr3")
    
    ## read outcome data
    outcomeData <- read.csv(paste(directory,"outcome-of-care-measures.csv",sep="/"), colClasses = "character")
    
    ## check state validity
    ## does the state exist in the data set? simplest
    if (!state %in% outcomeData$State ) {
        stop("invalid state")
    }
    
    ## check outcome validity
    ## valid outcomes heart attack heart failure, pneumonia
    ## on error throw new error with stop(), message - "invalid outcome"
    # include column numbers for each outcome
    validOutcomes <- data.frame( c(11,17,23),
                                 c("heart attack","heart failure", "pneumonia"))
    names(validOutcomes) <- c("col1", "col2")
    if (!outcome %in% validOutcomes[,2]) {
        stop("invalid outcome") 
    }
    
    # get the column number of the outcomeData data frame for the outcome
    outcomeCol <- validOutcomes[validOutcomes$col2==outcome, 1]
    
    ## num values are "best", "worst" or integer
    ## need a cast from numeric to integer?
    if (class(num) == "character"){
        if (num == "best") {        
            num <- 1 # get min or sort by column and name limit 1
        } else if (num == "worst") {
            # num would need to be converted to a function entirely
            # to use this technique. easier to add a conditional later
            # num <- function (x) nrow(x) # get the number of rows     
        } else {
            stop("invalid num")
        }
    }
    else if(class(num) == "numeric") {
        num <- as.integer(num)
        if (num <= 0) stop("invalid num") # 1 or higher
    }
    else if (class(num) == "integer" & num <= 0) {
        stop("invalid num") # 1 or higher
    }
    else if (class(num) != "integer"){
        stop("invalid num")
    }
 

    # state abbr is column 7
    # hospital name is column 2
    # outcome data column is selected in outcomeCol
    # get the state subset, and return data frame with 
    # hospital name and outcome column values 
    stateSubset <- subset(outcomeData, outcomeData[,7]==state & 
                                       !is.na(as.numeric(outcomeData[,outcomeCol])), 
                          select=c(2,outcomeCol))
    stateSubset <- data.frame(stateSubset[,1],as.numeric(stateSubset[,2]))
    # order ascending by outcome rank, then name
    stateSubset <- stateSubset[order(stateSubset[,2],stateSubset[,1]),]
    
   
    if (class(num)=="character" & num=="worst") {num <- nrow(stateSubset)}
    if (num > nrow(stateSubset)) return(NA)
    
    as.character(stateSubset[num,1])
    
    
    # find the value in the list, get the associated hostpital name
    # sort by name ascending
    #minStateSubset <- sort(stateSubset[stateSubset[2]==min(stateSubset[,2],na.rm=TRUE),1]) 
    #as.character(minStateSubset[1])
}