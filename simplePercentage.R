numAvailable <- 496
lifeListAccumulations <- NULL
cumLifeList <- NULL
currentYear <- 1

while (numAvailable > 0)
{
  t <- rbinom(1, numAvailable, 0.15)
  lifeListAccumulations <- c(lifeListAccumulations, t)
  numAvailable <- numAvailable - t
  if (currentYear == 1)
  {
    cumLifeList <- c(cumLifeList, t)
  }
  else
  {
    cumLifeList <- c(cumLifeList, cumLifeList[currentYear-1] + t)
  }
  
  currentYear <- currentYear + 1
}