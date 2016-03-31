#PCA Geometry demonstration
rm(list = ls())
gc()
library(ggplot2)
library(grid)

colrsO = c(1:360,1,150,360,250,250)
colrs = c(colrsO,1,70) 

#Step 1: build an ellipse
angles = (0:359)
angles = pi*angles/180
x.coords = c(2*(cos(angles) - sin(angles)),0,0,1,-1,0)
y.coords = c(cos(angles) + sin(angles), 0,1,0,0,-1)
original = data.frame( x = x.coords, y = y.coords)
origPlot = ggplot(data = original, aes(x, y)) +  
  geom_point(aes(colour = colrsO, size = 5)) + 
  scale_x_continuous(limits = c(-4,4)) + scale_y_continuous(limits = c(-4,4)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme(legend.position="none") +
  scale_colour_gradientn(colours=rainbow(4))
origPlot



#Step 2: rotate the ellipse so that the axes are no longer the best basis
rotator = matrix(data = c(sin(pi/6),cos(pi/6),cos(pi/6),-sin(pi/6)), nrow = 2)
#determinant(rotator, logarithm = F)
transformed = as.data.frame(as.matrix(original) %*% rotator)
transformed[366,] = c(0,1) #add in y-axis unit point
transformed[367,] = c(1,0) #add in x-axis unit point
colnames(transformed) = c("a","b") #name the new axes
transPlot = ggplot(data = transformed, aes(a, b)) +  geom_point(aes(colour = colrs, size = 5)) + 
  theme(legend.position="none") + 
  scale_x_continuous(limits = c(-4,4)) + scale_y_continuous(limits = c(-4,4)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + scale_colour_gradientn(colours=rainbow(4))
transPlot


#Step 3: add a little noise to the plot so PCA has something to do
dataformed = transformed + data.frame(a = runif(nrow(transformed),-.02,.02), b = runif(nrow(transformed),-.02,.02))
colnames(dataformed) = c("a_ish", "b_ish")
dataformed[366,] = c(0,1)
dataformed[367,] = c(1,0)
dataPlot = ggplot(data = dataformed, aes(a_ish, b_ish)) + 
  geom_point(aes(colour = colrs, size = 5)) + 
  scale_x_continuous(limits = c(-4,4)) + scale_y_continuous(limits = c(-4,4)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme(legend.position="none") +
  scale_colour_gradientn(colours=rainbow(4))
dataPlot


#Step 4: run PCA and plot the new basis vectors against the dataformed
#The new basis vectors (in the old space) are the columns of the loadings matrix
pcaOutput = princomp(dataformed, scores = T)
pcaMatrix = as.data.frame(pcaOutput$loadings[,])
center = c(mean(dataformed$a_ish), mean(dataformed$b_ish))
pcaPlot = dataPlot + theme(legend.position="none") +
  geom_segment(
    aes(
      x = center[1], 
      y = center[2], 
      xend = center[1] + pcaMatrix[1,1], 
      yend = center[2] + pcaMatrix[2,1],
      colour = 360), 
    arrow = arrow()) +
  geom_segment(
    aes(
      x = center[1], 
      y = center[2], 
      xend = center[1] + pcaMatrix[1,2], 
      yend = center[2] + pcaMatrix[2,2],
      colour = 150), 
    arrow = arrow())
pcaPlot
#Note that the new basis vectors terminate at unit points from the original ellipse



#Step 5: grab the "scores" from PCA and plot them
#This is the data reorganized into the new space
#Also provide the old basis vectors (rows of the loadings matrix)
newDim = as.data.frame(pcaOutput$scores)
newDimPlot = ggplot(data = newDim, aes(Comp.1, Comp.2)) + 
  geom_point(aes(colour = colrs, size = 5)) + 
  scale_x_continuous(limits = c(-4,4)) + scale_y_continuous(limits = c(-4,4)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme(legend.position="none") +
  scale_colour_gradientn(colours=rainbow(4)) +
  geom_segment(
    aes(
      x = 0, 
      y = 0, 
      xend = pcaMatrix[1,1], 
      yend = pcaMatrix[1,2],
      colour = 70), 
    arrow = arrow()) +
  geom_segment(
    aes(
      x = 0, 
      y = 0, 
      xend = pcaMatrix[2,1], 
      yend = pcaMatrix[2,2],
      colour = 1), 
    arrow = arrow())
newDimPlot
#Note that the new basis vectors terminate at unit points from the dataformed ellipse

