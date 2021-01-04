#####################
## Plot: Return, % ##
#####################

# Packages used:
library(ggplot2)

# Function:
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
