###################
## Data Reformat ##
###################

# Packages used:
library(lubridate)
library(quantmod)

# Function
reformat <- function(file){
  file$Date <- ymd(file$Date)
  file$Growth <- Delt(file$Close)*100
  for (i in file$Growth) {
    file$Change <- ifelse(file$Growth < 0, "Negative", "Positive")
  }
  file$Change <- factor(file$Change, levels= c("Negative", "Positive"))
  file <- data.frame(file$Date, file$Open, file$High, file$Low, file$Close, file$Volume, file$Growth, file$Change)
  colnames(file) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Growth","Change")
  file
}