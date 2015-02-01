rankhospital <- function(state,outcome,rank = "best"){
  
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
  
  #Return the hospital name with the rank position of 30-day death rate
  data <- data.frame(sameState[state])
  if(outcome == "heart attack"){
    data2 <- data.frame(name = data[,2], outcome = suppressWarnings(as.numeric(data[,11])))    
    valid_hospitals <- length(data2[!is.na(data2[,2]),2])
    
    position <- validRank(valid_hospitals,rank)
    if(!is.na(position)){
      sorted  <- data2[order(data2$outcome,data2$name),]
      return(as.character(sorted[position,1]))
    }
    else{
      return(NA)
    }
  }
  if(outcome == "heart failure"){
    data2 <- data.frame(name = data[,2], outcome = suppressWarnings(as.numeric(data[,17])))    
    valid_hospitals <- length(data2[!is.na(data2[,2]),2])
    
    position <- validRank(valid_hospitals,rank)
    if(!is.na(position)){
      sorted  <- data2[order(data2$outcome,data2$name),]
      return(as.character(sorted[position,1]))
    }
    else{
      return(NA)
    }
  }
  if(outcome == "pneumonia"){
    data2 <- data.frame(name = data[,2], outcome = suppressWarnings(as.numeric(data[,23])))    
    valid_hospitals <- length(data2[!is.na(data2[,2]),2])
    
    position <- validRank(valid_hospitals,rank)
    if(!is.na(position)){
      sorted  <- data2[order(data2$outcome,data2$name),]
      return(as.character(sorted[position,1]))
    }
    else{
      return(NA)
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

validRank <- function(size,rank){
  
#   if(rank != "best" && rank != "worst" && typeof(rank) != "numeric"){
#     stop("Rank should be either: best, worst, or an integer")
#   }

  if(rank == "best"){
    new_rank <- 1
    return(new_rank)
  }
  if(rank == "worst"){
    new_rank <- size
    return(new_rank)
  }
  if(rank > size){
    return(NA)
  }  
  
  return(rank)
}