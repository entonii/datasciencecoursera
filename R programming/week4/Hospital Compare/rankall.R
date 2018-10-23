rankall <- function(outcome, num) {
  ## Read data, cols: HospitalName, State, HeartAttack, HearFailure, Pneumonia
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")[,c(2,7,11,17,23)]
  
  ## Check that state and outcome are valid  
  if(! (outcome == "heart attack" || outcome == "heart failure" || outcome == "pneumonia") ) {
    stop("invalid outcome")
  }
  
  if(class(num) == "character"){
    if (! (num == "best" || num == "worst")){
      stop("invalid number")
    }
  }
  
  
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the state name
  
  # select the columns HospitalName, State, and outcome
  if(outcome == "heart attack") {
    data = data[,c(1,2,3)]
  } else if(outcome == "heart failure") {
    data = data[,c(1,2,4)]
  } else if(outcome == "pneumonia") {
    data = data[,c(1,2,5)]
  }
  names(data)[3] = "Deathrate"
  data[, 3] = suppressWarnings( as.numeric(data[, 3]) )
  
  # Remove rows with NA
  data = data[!is.na(data$Deathrate),]
  
  splited = split(data, data$State)
  ans = lapply(splited, function(x, num) {
    # Order by Deaths and then HospitalName
    x = x[order(x$Deathrate, x$Hospital.Name),]
    
    # Return
    if(class(num) == "character") {
      if(num == "best") {
        return (x$Hospital.Name[1])
      }
      else if(num == "worst") {
        return (x$Hospital.Name[nrow(x)])
      }
    }
    else {
      return (x$Hospital.Name[num])
    }
  }, num)
  
  #Return data.frame with format
  return ( data.frame(hospital=unlist(ans), state=names(ans)) )
}