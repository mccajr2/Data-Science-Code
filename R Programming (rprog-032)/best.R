best <- function(state, outcome) {
        
        ## Read outcome data
        csvoutput <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state and outcome are valid
        validStates <- unique(csvoutput$State)
        if(is.null(state) || !(state %in% validStates)) {
                stop('invalid state')
        }
        
        validOutcomes <- c("heart attack", "heart failure", "pneumonia")
        if(is.null(outcome) || !(outcome %in% validOutcomes)) {
                stop('invalid outcome')
        }
        
        ## Filter by state and outcome
        if(outcome=='heart attack'){
                col <- 11
        } else if(outcome=='heart failure'){
                col <- 17
        } else {
                col <- 23
        }
        fbs <- subset(csvoutput[c(2,7,col)],csvoutput$State==state)
        
        ## Rows that match minimum
        values <- suppressWarnings(as.numeric(fbs[,3]))
        minvalue <- min(values[!is.na(values)])
        rows <- subset(fbs,fbs[,3]==formatC(minvalue,digits=1,format="f"))
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        min(rows[,1])
}