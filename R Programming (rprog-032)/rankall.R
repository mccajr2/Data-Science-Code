rankall <- function(outcome, num = "best") {
        
        ## Read outcome data
        csvoutput <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = c("Not Available"))
        
        ## Check that outcome is valid
        validOutcomes <- c("heart attack", "heart failure", "pneumonia")
        if(is.null(outcome) || !(outcome %in% validOutcomes)) {
                stop('invalid outcome')
        }
        
        ## For each state, find the hospital of the given rank
        ## Filter by state and outcome
        if(outcome=='heart attack'){
                col <- 11
        } else if(outcome=='heart failure'){
                col <- 17
        } else {
                col <- 23
        }
        fbs <- csvoutput[c(2,7,col)]
        spl <- split(fbs, fbs[,2])
        
        if(num=="best"){
                num = 1
        } else if(num=="worst"){
                num = -1
        }
        
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        d <- lapply(spl, function(x,y) {
                
                # Order by rate and then by hospital name
                x <- x[ order(as.numeric(x[,3]), x[,1]), ]
                x <- na.omit(x)
                
                # Add rank column to data frame
                x$Rank <- seq.int(nrow(x))
                
                ## Return hospital name in that state with the given rank
                ## 30-day death rate
                if(y==-1){
                        hospital <- subset(x,x$Rank==nrow(x))
                        hospital <- hospital[,1]
                } else if(y > nrow(x)){
                        hospital <- NA
                } else{
                        hospital <- subset(x,x$Rank==y)
                        hospital <- hospital[,1]
                }
                hospital
        }, y=num)
        
        data.frame(hospital=unlist(d), state=names(d))
        
}