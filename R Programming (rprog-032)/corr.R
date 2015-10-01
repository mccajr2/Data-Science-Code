corr <- function(directory, threshold = 0) {
        
        c <- complete(directory)
        c <- subset(c,nobs>threshold)
        
        correlation <- vector()

        for(i in c$id) {
                f <- paste0(sprintf("%03d",i),".csv")
                f <- file.path(getwd(),"specdata",f,fsep =.Platform$file.sep)
                
                d <- read.csv(f)
                d <- na.omit(d)
                
                correlation <- append(correlation,cor(d$nitrate,d$sulfate))
        }
        
        correlation
}