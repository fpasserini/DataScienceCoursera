corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  corrvect <- vector("numeric");
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  for(i in 1:332){
    if(i < 10){
      filename <- paste("00",i,".csv", sep = "");
    } else if(i < 100) {
      filename <- paste("0",i,".csv", sep = "");
    } else {
      filename <- paste(i,".csv", sep = "");
    }
    file <- read.csv(paste(directory,filename, sep = "/"));
    filecomplete <- file[complete.cases(file),];
    if(nrow(filecomplete) > threshold){
      correlation <- cor(filecomplete[,"nitrate"],filecomplete[,"sulfate"]);
      corrvect[length(corrvect)+1] <- correlation;
    }
  }
  
  ## Return a numeric vector of correlations
  return(corrvect);
}
