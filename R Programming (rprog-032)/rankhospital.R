rankhospital <- function(state, outcome, num = "best") {
        
        ## Read outcome data
        csvoutput <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = c("Not Available"))
        
        ## Check that state and outcome are valid
        validStates <- unique(csvoutput$State)
        if(is.null(state) || !(state %in% validStates)) {
                stop('invalid state')
        }
        
        validOutcomes <- c("heart attack", "heart failure", "pneumonia")
        if(is.null(outcome) || !(outcome %in% validOutcomes)) {
                stop('invalid outcome')
        }
        
        ## Filter by state and outcome and omit NAs
        if(outcome=='heart attack'){
                col <- 11
        } else if(outcome=='heart failure'){
                col <- 17
        } else {
                col <- 23
        }
        fbs <- na.omit(subset(csvoutput[c(2,7,col)],csvoutput$State==state))
        
        # Order by rate and then by hospital name
        fbs <- fbs[ order(as.numeric(fbs[,3]), fbs[,1]), ]
        
        # Add rank column to data frame
        fbs$Rank <- seq.int(nrow(fbs))
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        if(num=="best"){
                num = 1
        } else if(num=="worst"){
                num = nrow(fbs)
        }
                        
        if(num > nrow(fbs)){
                hospital <- NA
        }
        else{
                hospital <- subset(fbs,fbs$Rank==num)
                hospital <- hospital[,1]
        }
        hospital
}