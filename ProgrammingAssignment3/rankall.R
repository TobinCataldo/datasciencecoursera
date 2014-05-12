## rankall that takes two arguments: an outcome name (outcome) and a 
## hospital ranking (num). 
## The function reads the outcome-of-care-measures.csv file and returns 
## a 2-column data frame containing the hospital in each state that has 
## the ranking specified in num. 
## For example the function call rankall("heart attack", "best") would 
## return a data frame containing the names of the hospitals that
## are the best in their respective states for 30-day heart attack death 
## rates. The function should return a value for every state (some may be 
## NA). The first column in the data frame is named hospital, which contains
## the hospital name, and the second column is named state, which contains 
## the 2-character abbreviation for the state name. Hospitals that do not 
## have data on a particular outcome should be excluded from the set of
## hospitals when deciding the rankings.

## > source("rankall.R")
## > head(rankall("heart attack", 20), 10)
## hospital state
## AK <NA> AK
## AL D W MCMILLAN MEMORIAL HOSPITAL AL
## AR ARKANSAS METHODIST MEDICAL CENTER AR
## AZ JOHN C LINCOLN DEER VALLEY HOSPITAL AZ
## CA SHERMAN OAKS HOSPITAL CA
## CO SKY RIDGE MEDICAL CENTER CO
## CT MIDSTATE MEDICAL CENTER CT
## DC <NA> DC
## DE <NA> DE
## FL SOUTH FLORIDA BAPTIST HOSPITAL FL
## > tail(rankall("pneumonia", "worst"), 3)
## hospital state
## WI MAYO CLINIC HEALTH SYSTEM - NORTHLAND, INC WI
## WV PLATEAU MEDICAL CENTER WV
## WY NORTH BIG HORN HOSPITAL DISTRICT WY
## > tail(rankall("heart failure"), 10)
## hospital state
## TN WELLMONT HAWKINS COUNTY MEMORIAL HOSPITAL TN
## TX FORT DUNCAN MEDICAL CENTER TX
## UT VA SALT LAKE CITY HEALTHCARE - GEORGE E. WAHLEN VA MEDICAL CENTER UT
## VA SENTARA POTOMAC HOSPITAL VA
## VI GOV JUAN F LUIS HOSPITAL & MEDICAL CTR VI
## VT SPRINGFIELD HOSPITAL VT
## WA HARBORVIEW MEDICAL CENTER WA
## WI AURORA ST LUKES MEDICAL CENTER WI
## WV FAIRMONT GENERAL HOSPITAL WV
## WY CHEYENNE VA MEDICAL CENTER WY

rankall <- function(outcome, num = "best") {
    ## Read outcome data
    ## Check that outcome and num are valid        
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
    source(paste(getwd(),"expandDirectory.R",sep="/"))
    directory <- expandDirectory("pr3")
    
    ## read outcome data
    outcomeData <- read.csv(paste(directory,"outcome-of-care-measures.csv",sep="/"), colClasses = "character")
    
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
    states <- sort(unique(outcomeData[,7]))
    

    #define the data frame and column row names
    df<- data.frame(character(0),character(0))
    df <- cbind(rep(NA,length(states)), states)
    colnames(df) <- c("hospital","state")
    row.names(df) <- states
    
    
    for (state in states) {
        myNum <- num
        # state abbr is column 7
        # hospital name is column 2
        # outcome data column is selected in outcomeCol
        stateSubset <- subset(outcomeData, outcomeData[,7]==state & 
                                  !is.na(as.numeric(outcomeData[,outcomeCol])), 
                              select=c(2,outcomeCol))
        stateSubset <- data.frame(stateSubset[,1],as.numeric(stateSubset[,2]))
        # order ascending by outcome rank, then name
        stateSubset <- stateSubset[order(stateSubset[,2],stateSubset[,1]),]
        
        
        if (class(myNum)=="character" & myNum=="worst") {
            myNum <- nrow(stateSubset)
        }
        if (myNum > nrow(stateSubset)) {
            df[state,"hospital"] <- NA
        } else {            
            df[state,"hospital"] <- as.character(stateSubset[myNum,1]) 
        }
    }
      
    as.data.frame(df)
}