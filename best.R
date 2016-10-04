best <- function(state, outcome) {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
    
    
    ## Check that state and outcome are valid
    if (!state %in% data$State){
        stop("invalid state")
    }
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    if (!outcome %in% outcomes){
        stop("invalid outcome")
    }
    
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    
    #get data column from outcome
    outcome.col <- function(x){
            if (x == "heart attack"){
                    column = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
            }
            else if (x == "heart failure") {
                    column = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
            }
            else if (x == "pneumonia"){
                    column = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
            }
            column
    }
    
    colname <- outcome.col(outcome)
    
    #get lowest rate from data column
    lowest.rate <- function(data, state){
            lowest <- min(data[data["State"] == state, colname], na.rm = TRUE)
            lowest
    }
    
    rate <- lowest.rate(data,state)
    
    filtered.data <- data[data["State"] == state & data[colname]== rate & !is.na(data[colname]),]
    hospitals <- sort(filtered.data["Hospital.Name"])
    hospitals$Hospital.Name[1]
    
}