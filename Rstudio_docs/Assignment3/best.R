best <- function(state,outcome){
  
  #Read data from the input file
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  sameState <- split(outcomes,outcomes$State)
  
  #Check that the state and the outcome are valid
  if(!stateValid(state,names(sameState))){
    stop("invalid state")
  }
  if(!outcomeValid(outcome)){
    stop("invalid outcome")
  }
  
  #Return the hospital name with lowest 30-day death rate
  data <- data.frame(sameState[state])
  if(outcome == "heart attack"){
    heart_attack <- suppressWarnings(as.numeric(data[,11]))
    arelow <- data[(heart_attack==min(heart_attack, na.rm = TRUE)),paste(state,"Hospital.Name",sep=".")]
    lowest_ones <- arelow[!is.na(arelow)]
    if(length(lowest_ones) != 1){
      return(comesFirst(lowest_ones))
    }
    else{
      return(lowest_ones)
    }
  }
  if(outcome == "heart failure"){
    heart_failure <- suppressWarnings(as.numeric(data[,17]))
    arelow <- data[(heart_failure==min(heart_failure, na.rm = TRUE)),paste(state,"Hospital.Name",sep=".")]
    lowest_ones <- arelow[!is.na(arelow)]
    if(length(lowest_ones) != 1){
      return(comesFirst(lowest_ones))
    }
    else{
      return(lowest_ones)
    }
  }
  if(outcome == "pneumonia"){
    pneumonia <- suppressWarnings(as.numeric(data[,23]))
    arelow <- data[(pneumonia==min(pneumonia, na.rm = TRUE)),paste(state,"Hospital.Name",sep=".")]
    lowest_ones <- arelow[!is.na(arelow)]
    if(length(lowest_ones) != 1){
      return(comesFirst(lowest_ones))
    }
    else{
      return(lowest_ones)
    }
  }
    
}

stateValid <- function(state,stateNames){
  for (i in 1:length(stateNames)){
    if(stateNames[i] == state)
      return(TRUE)
  }
  return(FALSE)
}

outcomeValid <- function(outcome){
  if(!(outcome == "heart attack")){
    if(!(outcome == "heart failure")){
      if(!(outcome == "pneumonia"))
        return(FALSE)
    }
  }
  return(TRUE)
}

comesFirst <- function(vector){
  return(sort(vector)[1])
}