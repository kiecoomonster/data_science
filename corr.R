corr <- function(directory, threshold = 0) {

    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files

    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0

    ## Return a numeric vector of correlations
    
    source("pollutantread.R")
    source("complete.R")
    com <- complete(directory)
    data <- com[com$nobs > threshold, ]
    result <- numeric(0)
    for(id in data$id) {
        csv <- pollutantread(directory, id)
        tf <- !is.na(csv$sulfate) & !is.na(csv$nitrate)
        x <- csv[tf, ]  
        result <- c(result, cor(x$sulfate, x$nitrate))
    }
    result
}