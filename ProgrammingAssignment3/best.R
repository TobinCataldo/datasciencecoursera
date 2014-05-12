## best that takes two arguments: the 2-character abbreviated name of a state 
## and an outcome name. The function reads the outcome-of-care-measures.csv file 
## and returns a character vectorwith the name of the hospital that has the best 
## (i.e. lowest) 30-day mortality for the specified outcome in that state. The 
## hospital name is the name provided in the Hospital.Name variable. The outcomes 
## can be one of heart attack, heart failure, or pneumonia. Hospitals that do 
## not have data on a particular outcome should be excluded from the set of 
## hospitals when deciding the rankings.

## Handling ties. If there is a tie for the best hospital for a given outcome, 
## then the hospital names should be sorted in alphabetical order and the 
## first hospital in that set should be chosen (i.e. if hospitals b, c,
## and f are tied for best, then hospital b should be returned).

## The function should check the validity of its arguments. 
## If an invalid state value is passed to best, the function should throw 
## an error via the stop function with the exact message invalid state. 
## If an invalid outcome value is passed to best, the function should throw 
## an error via the stop function with the exact message invalid outcome.
## Here is some sample output from the function.
##> source("best.R")
##> best("TX", "heart attack")
##[1] "CYPRESS FAIRBANKS MEDICAL CENTER"
##> best("TX", "heart failure")
##[1] "FORT DUNCAN MEDICAL CENTER"
##> best("MD", "heart attack")
##[1] "JOHNS HOPKINS HOSPITAL, THE"
##> best("MD", "pneumonia")
##[1] "GREATER BALTIMORE MEDICAL CENTER"
##> best("BB", "heart attack")
##Error in best("BB", "heart attack") : invalid state
##> best("NY", "hert attack")
##Error in best("NY", "hert attack") : invalid outcome

best <- function(state, outcome) {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    
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
    
    # state abbr is column 7
    # hospital name is column 2
    # outcome data column is selected in outcomeCol
    # get the state subset, and return data frame with 
    # hospital name and outcome column values 
    stateSubset <- subset(outcomeData, outcomeData[,7]==state, select=c(2,outcomeCol))
    stateSubset <- data.frame(stateSubset[,1],as.numeric(stateSubset[,2]))
    # find the value in the list, get the associated hostpital name
    # sort by name ascending
    minStateSubset <- sort(stateSubset[stateSubset[2]==min(stateSubset[,2],na.rm=TRUE),1]) 
    as.character(minStateSubset[1])
    
    ############
    # An alternative 
    
    # state abbr is column 7
    # hospital name is column 2
    # outcome data column is selected in outcomeCol
    # get the state subset, and return data frame with 
    # hospital name and outcome column values 
#    stateSubset <- subset(outcomeData, outcomeData[,7]==state & 
#                              !is.na(as.numeric(outcomeData[,outcomeCol])), 
#                          select=c(2,outcomeCol))
#    stateSubset <- data.frame(stateSubset[,1],as.numeric(stateSubset[,2]))
    # order ascending by outcome rank, then name
#    stateSubset <- stateSubset[order(stateSubset[,2],stateSubset[,1]),]
    
#    if (class(num)=="character" & num=="worst") {num <- nrow(stateSubset)}
#    if (num > nrow(stateSubset)) return(NA)
    
#    as.character(stateSubset[num,1])
}