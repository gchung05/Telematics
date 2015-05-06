# Condensed Code For Blog Post
# Author: Gary Chung
# ============================

# Required Libraries
  library(ggplot2)
  library(dplyr)
  library(png)

# Load vehicle data
  dataFolder <- paste0(getwd(),"/../Documents/Telematics Data/")
  load(paste0(dataFolder, "vehicle1.Rdata"))
  load(paste0(dataFolder, "vehicle2.Rdata"))
  load(paste0(dataFolder, "vehicle3.Rdata"))

# ggplot2 options
  # Blank background
    blankBG <- theme(axis.line=element_blank(),
                axis.text.x=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks=element_blank(),
                axis.title.x=element_blank(),
                axis.title.y=element_blank(),
                legend.position="none",
                panel.background=element_rect(fill = "transparent",colour = NA),
                panel.border=element_blank(),
                panel.grid.major=element_blank(),
                panel.grid.minor=element_blank(),
                plot.background = element_rect(fill = "transparent",colour = NA))
  # Dimensions and colors
    coordsAndColors <- list(xlim(-1000,5000), 
                            ylim(-2500,2500),
                            scale_colour_gradient(low="red",high="green"))

# Example 1: Plot Vehicle 1, Trip 20
# ---------------------------------

  # Select the vehicle-trip
    currentTrip <- vehicle1[[20]]
  # Save the last recorded x-y point
    lastPoint <- currentTrip[nrow(currentTrip),]
  # θ is saved as: 
  #  Fit a least-squares regression to the data %>% 
  #    Predict y (ŷ) for the last recorded x-value (x) %>%
  #       Take the arc-tangent of ŷ and x
    theta <-  lm(y~x, currentTrip) %>% 
                predict(lastPoint) %>% 
                  atan2(lastPoint$x)
  # Rotate the vehicle-trip so the orientation is left to right
    currentTrip$xN <- cos(-theta)*currentTrip$x - sin(-theta)*currentTrip$y
    currentTrip$yN <- sin(-theta)*currentTrip$x + cos(-theta)*currentTrip$y
  # Calculate the distance between two successive points. 
  # Based on our ~equally spaced time assumption, this distance is a good proxy for speed.
    currentTrip$speed <- c(NA, sqrt(diff(currentTrip$x,1)^2 + diff(currentTrip$y,1)^2)) 
  # Plot the rotated trip
    png("d1t1.png", width=600, height=600)
    ggplot(currentTrip, aes(xN, yN, color=speed)) + geom_path(size=2) + coordsAndColors + blankBG
    dev.off()
  # Clean up
    rm(currentTrip, lastPoint, theta)


# FUNCTION: Create ggplot object for a given vehicle and trip
# ----------------------------------------------------------

  plotTrip <- function(tripNum, vehicle, lineThickness) {
    currentTrip <- vehicle[[tripNum]]
    lastPoint <- currentTrip[nrow(currentTrip),]
    theta <-  lm(y~x, currentTrip) %>% predict(lastPoint) %>% atan2(lastPoint$x)
    currentTrip$xN <- cos(-theta)*currentTrip$x - sin(-theta)*currentTrip$y
    currentTrip$yN <- sin(-theta)*currentTrip$x + cos(-theta)*currentTrip$y
    currentTrip$speed <- c(NA, sqrt(diff(currentTrip$x,1)^2 + diff(currentTrip$y,1)^2)) 
    ggplot(currentTrip, aes(xN, yN, color=speed)) + geom_path(size=lineThickness) + coordsAndColors + blankBG
  }


# FUNCTION: Multiple plot function
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


# Example 2: Plot Vehicles, All 200 Trips
# -----------------------------------------

  # Initiate the list of arguments to the multiplot function
  # cols sets the number of columns in the multiplot grid
    allPlots <- list(cols=20)
  # Generate the 200 ggplots as a list of arguments to be passed to the multiplot function
  # For Vehicle 1
    for(i in 1:200) { allPlots[[i+1]] <- plotTrip(i, vehicle1, 0.1) }
  # Save as a PNG
    png("d1all200.png", width=1600, height=800)
    do.call(multiplot, allPlots)
    dev.off()
  # For Vehicle 2
    allPlots <- list(cols=20)
    for(i in 1:200) { allPlots[[i+1]] <- plotTrip(i, vehicle2, 0.1) }
    png("d2all200.png", width=1600, height=800)
    do.call(multiplot, allPlots)
    dev.off()
  # For Vehicle 3
    allPlots <- list(cols=20)
    for(i in 1:200) { allPlots[[i+1]] <- plotTrip(i, vehicle3, 0.1) }
    png("d3all200.png", width=1600, height=800)
    do.call(multiplot, allPlots)
    dev.off()
  # Clean up
    rm(i, allPlots)


# Example 3: Clustering Vehicle-Trips
# -----------------------------------
  library(Rtsne)

  # FUNCTION: Save images, run t-SNE on images, plot t-SNE clusters
  # ---------------------------------------------------------------

    clusterVehicle <- function(vehicle, vehicleNum) {
      # Save each vehicle-trip as a png image
        for(i in 1:200) {
          ggsave(plotTrip(i, vehicle, 1), width=3, height=3, units="in", dpi=50, bg = "transparent",
                 file=paste0(getwd(), "/../Documents/Telematics Data/", vehicleNum, "/trip", i, ".png"))
        }
        
      # Set the matrix of data, where rows are the vehicle trips and columns are the individual grayscale pixel values
      trips <- matrix(nrow=200, ncol=22500)
      
      # Read in every image to the matrix
        for(i in 1:200) {
          currentPNG <- readPNG(paste0(getwd(), "/../Documents/Telematics Data/", vehicleNum, "/trip", i, ".png"))
          grayV <- c(0.2126*currentPNG[,,1] + 0.7152*currentPNG[,,2] + 0.0722*currentPNG[,,3]) # Conversion to grayscale vector
          trips[i,] <- grayV
        }
      
      # Remove duplicate trips
        obsNoDupes <- seq(1:200)[!duplicated(data.frame(trips))]
        trips <- trips[obsNoDupes,] 
      
      # Run t-SNE
        tsne <- Rtsne(trips, initial_dims=5, perplexity=20, theta=0.1)
      
      # Plot t-SNE
        # Save a higher resolution version of each trip to use in the plot overlay
          for(i in 1:200) {
            ggsave(plotTrip(i, vehicle, 1), width=3, height=6, units="in", dpi=100, bg = "transparent",
                   file=paste0(getwd(), "/../Documents/Telematics Data/", vehicleNum, "/trip", i, ".png"))
          }
        # Plot
          png(paste0("d",vehicleNum,"tsne.png"), width=1600, height=800)
          plot(tsne$Y, axes=F, xlab=NA, ylab=NA, pch=" ")
          for(i in 1:nrow(trips)) {
            rasterImage(readPNG(paste0(getwd(), "/../Documents/Telematics Data/", vehicleNum, "/trip", obsNoDupes[i], ".png"), native=TRUE), 
                        tsne$Y[i,1]-3, tsne$Y[i,2]-3, tsne$Y[i,1]+3, tsne$Y[i,2]+3)
          }
          dev.off() 
    }


  # Run t-SNE for each vehicle
    clusterVehicle(vehicle1, 1)
    clusterVehicle(vehicle2, 2)
    clusterVehicle(vehicle3, 3)
