complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  completedata <- data.frame(id=integer(),nobs=integer());
  cdnames <- names(completedata);
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used

  for(i in id){
    if(i < 10){
      filename <- paste("00",i,".csv", sep = "");
    } else if(i < 100) {
      filename <- paste("0",i,".csv", sep = "");
    } else {
      filename <- paste(i,".csv", sep = "");
    }
    file <- read.csv(paste(directory,filename, sep = "/"));
    temp <- nrow(file[complete.cases(file),]);
    completedata[nrow(completedata)+1,] <- c(i,temp);
  }
  
  names(completedata) <- cdnames;
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  return(completedata); 
}