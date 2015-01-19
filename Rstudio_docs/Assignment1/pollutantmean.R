pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  summm <- 0;
  lengthhh <- 0;
  
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
    #file <- read.csv(filename);
    temp <- file[!is.na(file[,pollutant]),pollutant]
    summm <- summm + sum(temp);
    lengthhh <- lengthhh + length(temp);
  }
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  summm/lengthhh;
}