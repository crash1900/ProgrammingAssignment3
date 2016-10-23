rankhospital <- function(state, outcome, num = "best")
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
  filteredStates <- outcomeData[outcomeData[,7] == state,]
  
  ## Get subset of filtered states and give them column names
  filteredSet <- filteredStates[,c(2, 7, col)]
  colnames(filteredSet) <- c('hospital', 'state', 'outcome')
  
  ## Remove NAs from results before getting the minimum of the desire outcome
  result <- na.omit(filteredSet)
  
  ## Order by result and hospital
  result <- result[order(result$outcome, result$hospital),]
  
  ## Return the ranked results based on the input number
  if (num == 'best')
  {
    result <- result[1, ]
  }
  else if (num == 'worst')
  {
    result <- result[nrow(result), ]
  }
  else
  {
    result <- result[num,]
  }
  result$hospital
}

## Tests
## rankhospital("TX", "heart failure", 4)
## DETAR HOSPITAL NAVARRO
## rankhospital("MD", "heart attack", "worst")
## HARFORD MEMORIAL HOSPITAL
## rankhospital("MN", "heart attack", 5000)
## NA