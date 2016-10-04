rankhospital <- function(state, outcome, num = "best"){
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
        outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
        
        #1. distill data into 3 relevant columns
        #2. select data only for that state
        #3. sort by lowest outcome, then alphabetically by hospital
        hso <- data[, c(2, 7, outcomes[outcome])]
        hso_state <- hso[hso[2] == state & !is.na(hso[,3]),]
        hso_sorted <- hso_state[order(hso_state[,3], hso_state[,1]),]
        
        #determine rank index
        if (num == "best"){
                num <- 1
        }
        if (num == "worst"){
                num <- nrow(hso_sorted)
        }
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        hso_sorted[num, 1]
        
}