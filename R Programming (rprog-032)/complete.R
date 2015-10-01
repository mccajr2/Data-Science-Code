complete <- function(directory, id = 1:332) {

        counts = vector(mode="integer")
        
        for(i in id) {
                f <- paste0(sprintf("%03d",i),".csv")
                f <- file.path(getwd(),"specdata",f,fsep =.Platform$file.sep)
                
                d <- read.csv(f)
                
                counts <- append(counts,nrow(na.omit(d)))
        }
        
        data.frame(id=id,nobs=counts)
        
}