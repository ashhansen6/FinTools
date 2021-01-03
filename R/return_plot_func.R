#####################
## Plot: Return, % ##
#####################

# Packages used:
library(ggplot2)

# Functions:

## Return Plot v1
return.plot <- function(symbol, palette){
  
  ggplot(symbol) +
    geom_point(aes(x= symbol$Date, y= symbol$Growth, color= symbol$Change)) +
    geom_line(aes(x= symbol$Date, y= symbol$Growth)) +
    labs(title= "Daily Rate of Return") +
    xlab("Time") +
    ylab("Return, decimal form")
}

## Return Plot v2
plot.return <- function(symbol, start, end, title, timestamp){
  ggplot(symbol[symbol$Date >= start & symbol$Date < end, 1:8])+
    geom_col(aes(x= Date, y= Growth, fill= Change)) +
    labs(title= title, x= timestamp, y= "Return, %") +
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
}

## Return Plot v3
return.plot3 <- function(symbol, start, end, title, timestamp){
  ggplot(symbol[symbol$Date >= start & symbol$Date < end, 1:8]) +
    geom_area(aes(x= Date, y= Growth), color= "lightblue") +
    labs(title= title, x= timestamp, y= "Return, %") +
    theme(panel.background= element_rect(fill= "black",
                                         color= "darkgrey",
                                         linetype= "solid",
                                         size= 0.5),
          panel.grid.major= element_line(color= "darkgrey",
                                         linetype= "solid",
                                         size= 0.5),
          panel.grid.minor= element_line(color= "darkgrey",
                                         linetype= "solid",
                                         size= 0.25))
}

