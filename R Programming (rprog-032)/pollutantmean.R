pollutantmean <- function(directory, pollutant, id = 1:332) {
        
        data <- vector(mode="numeric")
        
        for(i in id) {
                f <- paste0(sprintf("%03d",i),".csv")
                f <- file.path(getwd(),"specdata",f,fsep =.Platform$file.sep)
                
                d <- read.csv(f)
                
                pdata <- d[,c(pollutant)]
                pdata <- pdata[!is.na(pdata)]
                
                data <- append(pdata, data)
        }
        
        mean(data)
}