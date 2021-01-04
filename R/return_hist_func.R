######################
## Return Histogram ##
######################

# Packages used:
library(tidyverse)

# Function
hist.return <- function(symbol, start, end, title, by.change = FALSE, strata = "both"){
  hist_list <- list()
  if (by.change == FALSE){
    base <- ggplot(symbol[symbol$Date >= start & symbol$Date < end, 1:8]) +
      geom_histogram(aes(x= Growth, y= ..density..), color= "darkgrey") +
      labs(title= title, x= "Return, %", y= "Density") +
      scale_fill_manual(values= c("cyan")) +
      theme(panel.background= element_rect(fill= "black",
                                           color= "grey",
                                           linetype= "solid",
                                           size= 0.5),
            panel.grid.major= element_line(color= "grey",
                                           linetype= "solid",
                                           size= 0.5),
            panel.grid.minor= element_line(color= "grey",
                                           linetype= "solid",
                                           size= 0.25))
    return(base)
  } else {
    strat_hist <- list()
    strat <- ggplot(symbol[symbol$Date >= start & symbol$Date < end, 1:8]) +
      geom_histogram(aes(x= Growth, y= ..density.., fill= Change), color= "darkgrey") +
      labs(title= title, x= "Return, %", y= "Density") +
      scale_fill_manual(values= c("#FF3333", "#00FF00")) +
      theme(panel.background= element_rect(fill= "black",
                                           color= "grey",
                                           linetype= "solid",
                                           size= 0.5),
            panel.grid.major= element_line(color= "grey",
                                           linetype= "solid",
                                           size= 0.5),
            panel.grid.minor= element_line(color= "grey",
                                           linetype= "solid",
                                           size= 0.25))
    strat_pos <- ggplot(symbol[symbol$Date >= start & symbol$Date < end & symbol$Change == "Positive", 1:8]) +
      geom_histogram(aes(x= Growth, y= ..density..), color= "darkgrey", fill= "#00FF00") +
      labs(title= title, x= "Return, %", y= "Density") +
      theme(panel.background= element_rect(fill= "black",
                                           color= "grey",
                                           linetype= "solid",
                                           size= 0.5),
            panel.grid.major= element_line(color= "grey",
                                           linetype= "solid",
                                           size= 0.5),
            panel.grid.minor= element_line(color= "grey",
                                           linetype= "solid",
                                           size= 0.25))
    strat_neg <- ggplot(symbol[symbol$Date >= start & symbol$Date < end & symbol$Change == "Negative", 1:8]) +
      geom_histogram(aes(x= Growth, y= ..density..), color= "darkgrey", fill= "#FF3333") +
      labs(title= title, x= "Return, %", y= "Density") +
      theme(panel.background= element_rect(fill= "black",
                                           color= "grey",
                                           linetype= "solid",
                                           size= 0.5),
            panel.grid.major= element_line(color= "grey",
                                           linetype= "solid",
                                           size= 0.5),
            panel.grid.minor= element_line(color= "grey",
                                           linetype= "solid",
                                           size= 0.25))
    stat_list <- list(strat, strat_pos, strat_neg)
    names(stat_list) <- c("both", "positive", "negative")
    return(stat_list[[strata]])
  }
}
