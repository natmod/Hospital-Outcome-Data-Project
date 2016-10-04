rankall <- function(outcome, num = "best") {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
        
        ## Check that outcome is valid
        outcomes <- c("heart attack", "heart failure", "pneumonia")
        if (!outcome %in% outcomes){
                stop("invalid outcome")
        }
        outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
        
        #1. distill data into 3 relevant columns
        #2. order by lowest outcome, then hospital name, then state
        #3. split dataframe into list of state dataframes
        hso <- data[, c(2, 7, outcomes[outcome])]
        hso_sorted <- hso[order(hso[,3], hso[,1], hso[,2]),]
        hso_states <- split(hso_sorted, hso_sorted$State)
        
        #pick_rank sub-function for lapply
        pick_rank <- function(x, rank) {
                if (rank == "best"){
                        rank <- 1
                }
                if (rank == "worst"){
                        y <- x[!is.na(x[,3]),]
                        rank <- nrow(y)
                }
                picked <- x[rank, 1:2]
                #make sure state is not NA
                picked[[1,2]] <- x[[1,2]]
                picked
        }
        
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        results_list <- lapply(hso_states, pick_rank, rank = num)
        result <- setNames(do.call(rbind.data.frame, results_list), c("hospital", "state"))
        
}