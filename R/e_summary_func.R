######################
## Enhanced Summary ##
######################

# For numerical data;
# Five number summary with variance and sd

e.summ <- function(data){
  Summary <- summary(data)
  Variance <- var(data)
  Standard_Deviation <- sd(data)
  summ <- list(Summary, Variance, Standard_Deviation)
  names(summ) <- c("Summary", "Variance", "Standard Deviation")
  return(summ)
}