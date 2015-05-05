# Utility Functions
# *****************
library(ggplot2)
library(animation)
library(RColorBrewer)
library(dplyr)
library(ROCR)
library(png)
library(Rtsne)

# Function to rotate/normalize trip trajectory, then plot with color to indicate speed.
# -------------------------------------------------------------------------------------
plotNorm <- function(tripNum) {
  # Select a driver-trip
  temp <- dSet[[tripNum]]
  
  # Best fit line -> Use to predict final point -> Find the angle
  tempR <- lm(y~x, temp) %>% predict(temp[nrow(temp),]) %>% atan2(temp[nrow(temp),]$x)
  
  # Rotate the trip to approximately align with all other trips
  temp$xN <- cos(-tempR)*temp$x - sin(-tempR)*temp$y
  temp$yN <- sin(-tempR)*temp$x + cos(-tempR)*temp$y
  
  # The distance calculation is a proxy for speed, which will be used for the color of the line
  temp$speed <- c(NA, sqrt(diff(temp$x,1)^2 + diff(temp$y,1)^2)) 
  
  # Plot the rotated trip
  ggplot(temp, aes(xN, yN, color=speed)) + geom_path(size=2) + xlim(-1000,6000) + ylim(-3000,3000) +
    scale_colour_gradient(low = "#E0E0E0", high = "#000000") +
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none",
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank())
}


# Function for Driver X, all 200 trips in a list
# ----------------------------------------------
make_dta <- function(driver) {
  hold <- list()
  for(i in 1:200) {
    hold[[i]] <- data.frame(cbind(driver_trip=paste("11_", i, sep=""), read.csv(paste("SampleSet/11/", i, ".csv", sep=""))))
  }
  return(hold)
}


# Multiple plot function
# Source: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
# -------------------------------------------------------------------------------
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}