best <- function(state, outcome)
{
  ## Read outcome data
  outcomeData <- read.csv('outcome-of-care-measures.csv', na.strings = 'Not Available', stringsAsFactors = FALSE)
  
  
  ## Check that state and outcome are valid
  validState = state %in% unique(outcomeData$State)
  if (!validState)
  {
    stop('invalid state')
  }
  
  col <- -1
  
  if (tolower(outcome) == 'heart attack')
  {
    col <- 11
  }
  else if (tolower(outcome) == 'heart failure')
  {
    col <- 17
  }
  else if (tolower(outcome) == 'pneumonia')
  {
    col <- 23
  }
  else
  {
    stop('invalid outcome')
  }
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  ## Filter the states first
  #statesFiltered <- outcomeData[outcomeData$state == state & outcomeData[,col] != 'Not Available', ]
  filteredStates <- outcomeData[outcomeData[,7] == state,]
  result <- filteredStates[,c(2, 7, col)]
  colnames(result) <- c('hospital', 'state', 'outcome')
  
  result <- na.omit(result)
  result <- result[which.min(result$outcome),]
  
  ## Sort the results by hospital and return only the first of the results
  ## if there is a tie
  result <- result[order(result$hospital),]
  head(result$hospital, 1)
}


## Tests
## best("TX", "heart attack")
## CYPRESS FAIRBANKS MEDICAL CENTER
## best("TX", "heart failure")
## FORT DUNCAN MEDICAL CENTER
## best("MD", "heart attack")
## JOHNS HOPKINS HOSPITAL, THE
## best("MD", "pneumonia")
## GREATER BALTIMORE MEDICAL CENTER
