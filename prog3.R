best <- function(state, outcome)
{
  ## Read outcome data
  outcomeData <- read.csv('outcome-of-care-measures.csv', na.strings = 'Not Available', stringsAsFactors = FALSE)
  
  
  ## Check that state and outcome are valid
  validState = state %in% unique(outcomeData$State)
  validOutcome <- FALSE
  col <- -1
  
  if (tolower(outcome) == 'heart attack')
  {
    col <- 11
    validOutcome <- TRUE
  }
  else if (tolower(outcome) == 'heart failure')
  {
    col <- 17
    validOutcome <- TRUE
  }
  else if (tolower(outcome) == 'pneumonia')
  {
    col <- 23
    validOutcome <- TRUE
  }
  
  if (!validState)
  {
    stop('invalid state')

  }
  else if (!validOutcome)
  {
    stop('invalid outcome')
  }
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  ## Filter the states first
  #statesFiltered <- outcomeData[outcomeData$state == state & outcomeData[,col] != 'Not Available', ]
  store <- outcomeData[outcomeData[,7] == state,]
  store2 <- store[,c(2, 7, col)]
  colnames(store2) <- c('hospital', 'state', 'outcome')
  
  store3 <- na.omit(store2)
  store3 <- store3[which.min(store3$outcome),]
  
  ## Sort the results by hospital and return only the first of the results
  ## if there is a tie
  store3 <- store3[order(store3$hospital),]
  head(store3$hospital, 1)
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
