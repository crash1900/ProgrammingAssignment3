rankall <- function(outcome, num="best")
{
  ## Read outcome data
  outcomeData <- read.csv('outcome-of-care-measures.csv', na.strings = 'Not Available', stringsAsFactors = FALSE)
  
  ## Check that outcome is valid
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
  
  ## For each state, find the hospital of the given rank
  
  ## Limit columns to what we're interested in, omitting NA values
  filteredSet <- outcomeData[, c(2, 7, col)]
  colnames(filteredSet) <- c('hospital', 'state', 'outcome')
  result <- na.omit(filteredSet)

  ## Split the result by state
  splitResult <- split(result, result$state)
  
  ## Parse the split results through a function that
  ## returns a hospital name with the state as the index
  fullList <- lapply(splitResult, function(mydata, n){
    mydata = mydata[order(mydata$outcome, mydata$hospital), ]
    if (num == 'best')
    {
      result <- mydata[1,]
      result$hospital
    }
    else if (num == 'worst')
    {
      result <- mydata[nrow(mydata),]
      result$hospital
    }
    else
    {
      result <- mydata[num, ]
      result$hospital
    }
  }, num)

  ## Assemble data frame from the unlisted values
  data.frame(hospital = unlist(fullList), state = names(fullList))
}
