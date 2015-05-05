# t-SNE / clustering analysis of imaged trajectories
# --------------------------------------------------
source(paste0(getwd(),"/","utilityFunctions.R")) # All library calls and custom functions contained here.
#source(paste0(getwd(),"/","dataSources.R")) # Non-essential external data sources read into memory are here.

# Set driver by driver # (Function make_dta saved in UtilityFunctions.R)
  dSet <- make_dta(12)

# Just for fun, plot driver-trip trajectories for Driver 11
# ---------------------------------------------------------

  # Just subset to 5 driver trips to make the plot easier to see.
    dSet_subset <- rbind(dSet[[1]],dSet[[2]],dSet[[3]],dSet[[4]],dSet[[5]])
  # Plot it.
    ggplot(dSet_subset, aes(x, y, color=driver_trip)) + geom_point() +
      geom_path() + theme_minimal() + scale_fill_brewer(palette="Set3")

  # Animate the driver-trip trajectories. Should give a sense of speed.
    # Draw the scatterplot, but only up to the cutoff row we set
      draw.curve<-function(cutoff){
        a <-  ggplot(dSet_subset, aes(x, y, color=driver_trip)) + geom_point(data=dSet_subset[1:cutoff,]) +
          geom_path(data=dSet_subset[1:cutoff,]) + theme_minimal() + scale_fill_brewer(palette="Set3") +
          xlim(-5000,5000) + ylim(-5000,5000)
        print(a)
      }
    # Draw scatterplot up to row 1000
      #draw.curve(cutoff=1000)
    # Iterate over all the rows in the dataset
      trace.animate <- function() {
        lapply(seq(1:nrow(dSet_subset)), function(i) {draw.curve(i)})
      }
    # Save the animation to file
      saveGIF(trace.animate(), interval = .0001, movie.name="dta11_1_5.gif")


# Select a driver and draw its rotated/normalized trajectories
# ---------------------------------------------------------

# (Function multiplot saved in UtilityFunctions.R)
multiplot(plotNorm(1), plotNorm(2), plotNorm(3), plotNorm(4), plotNorm(5), plotNorm(6), plotNorm(7), plotNorm(8), plotNorm(9), plotNorm(10),
          plotNorm(11), plotNorm(12), plotNorm(13), plotNorm(14), plotNorm(15), plotNorm(16), plotNorm(17), plotNorm(18), plotNorm(19), plotNorm(20), 
          plotNorm(21), plotNorm(22), plotNorm(23), plotNorm(24), plotNorm(25), plotNorm(26), plotNorm(27), plotNorm(28), plotNorm(29), plotNorm(30), 
          plotNorm(31), plotNorm(32), plotNorm(33), plotNorm(34), plotNorm(35), plotNorm(36), plotNorm(37), plotNorm(38), plotNorm(39), plotNorm(40), 
          plotNorm(41), plotNorm(42), plotNorm(43), plotNorm(44), plotNorm(45), plotNorm(46), plotNorm(47), plotNorm(48), plotNorm(49), plotNorm(50),
          plotNorm(51), plotNorm(52), plotNorm(53), plotNorm(54), plotNorm(55), plotNorm(56), plotNorm(57), plotNorm(58), plotNorm(59), plotNorm(60),
          plotNorm(61), plotNorm(62), plotNorm(63), plotNorm(64), plotNorm(65), plotNorm(66), plotNorm(67), plotNorm(68), plotNorm(69), plotNorm(70),
          plotNorm(71), plotNorm(72), plotNorm(73), plotNorm(74), plotNorm(75), plotNorm(76), plotNorm(77), plotNorm(78), plotNorm(79), plotNorm(80),
          plotNorm(81), plotNorm(82), plotNorm(83), plotNorm(84), plotNorm(85), plotNorm(86), plotNorm(87), plotNorm(88), plotNorm(89), plotNorm(90),
          plotNorm(91), plotNorm(92), plotNorm(93), plotNorm(94), plotNorm(95), plotNorm(96), plotNorm(97), plotNorm(98), plotNorm(99), plotNorm(100),
          cols=10)

# Show/save plots of all trips for a given driver
# ---------------------------------------------------------

# Set driver by driver # (Function make_dta saved in UtilityFunctions.R)
dSet <- make_dta(15)

plotNorm(15)
for(i in 1:200) {
  ggsave(plotNorm(i), width=3, height=3, units="in", dpi=25, 
         file=paste0("/Users/gunkadoodah/Desktop/Telemetric/", "trip", i, ".png"))
}

tripsMatrix <- matrix(nrow=200, ncol=5625)
for(i in 1:200) {
  hold <- readPNG(paste0("/Users/gunkadoodah/Desktop/Telemetric/", "trip", i, ".png"))
  holdV <- c(0.2126*hold[,,1] + 0.7152*hold[,,2] + 0.0722*hold[,,3]) # To grayscale vector
  tripsMatrix[i,] <- holdV
}
rm(hold, holdV)

# temp <- readPNG(paste0("/Users/gunkadoodah/Desktop/Telemetric/", "trip", 15, ".png"), native=TRUE)
# temp <- 0.2126*temp[,,1] + 0.7152*temp[,,2] + 0.0722*temp[,,3] # To grayscale matrix
# temp <- readPNG(paste0("/Users/gunkadoodah/Desktop/Telemetric/", "trip", 15, ".png"))

# Run t-SNE
  tsne <- Rtsne(tripsMatrix, initial_dims=2, perplexity=20, theta=0.1)

# Heirarchal clustering
  hclD<-dist(tsne$Y)
  hcl<-hclust(hclD, method="ward.D")
  hcl2 <- cutree(hcl, k=2)
  #   plot(tsne$Y, axes=F, xlab=NA, ylab=NA, col="white")
  #   text(tsne$Y[,1], tsne$Y[,2], hcl2)

# K-means clustering
  #   temp<-kmeans(tsne$Y, 2)
  #   library(cluster)
  #   clusplot(tsne$Y, temp$cluster, color=TRUE, shade=TRUE, 
  #            labels=2, lines=0)

# Plot the trips
  plot(tsne$Y, axes=F, xlab=NA, ylab=NA, pch=20, col=hcl2+1, cex=10, main="Driver 15")
  for(i in 1:200) {
    rasterImage(readPNG(paste0("/Users/gunkadoodah/Desktop/Telemetric/", "trip", i, ".png"), native=TRUE), 
                tsne$Y[i,1]-1.5, tsne$Y[i,2]-1.5, tsne$Y[i,1]+1.5, tsne$Y[i,2]+1.5)
  }




#===================================================
