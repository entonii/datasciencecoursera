best <- function(state, outcome) {
  ## Read data, columns: HospitalName, State, HeartAttack, HearFailure, Pneumonia
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")[,c(2,7,11,17,23)]
  
  ## Check that state and outcome are valid  
  if(! ( state %in% levels(factor(data$State)) ) ) {
    stop("invalid state")
  }
  
  if(! (outcome == "heart attack" || outcome == "heart failure" || outcome == "pneumonia") ) {
    stop("invalid outcome")
  }
  
  ## Return the best hospital in the selected state
  
  # Screening by state
  data = data[data$State==state,]
  # Remove the state column after screening the data by state
  data = data[,c(1,3,4,5)]
  
  # Screening by outcome
  if(outcome == "heart attack") {
    data = data[,c(1,2)]
  } else if(outcome == "heart failure") {
    data = data[,c(1,3)]
  } else if(outcome == "pneumonia") {
    data = data[,c(1,4)]
  }
  names(data)[2] = "Deathrate"
  data[, 2] = suppressWarnings( as.numeric(data[, 2]) )
  
  # Remove rows with NA
  data = data[!is.na(data$Deathrate),]
  
  # Order by Deathrate and then HospitalName
  data = data[order(data$Deathrate, data$Hospital.Name),]
  
  # Return
  return (data$Hospital.Name[1])
}