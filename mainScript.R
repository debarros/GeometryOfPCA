#PCA Geometry demonstration

# Load libraries and functions ####
source("functions.R")

# Build a shape.  ####
# The given example is an ellipse 
# In the fPoints.R file, there is code to build the letter F
angles = (0:359)
angles = pi*angles/180
x.coords = 3*cos(angles)
y.coords = 2*sin(angles)
original = addKeyPoints(data.frame( x = x.coords, y = y.coords))

#Create the necessary colors, shapes, and sizes for the points ####
colrsO = c(rep("black",nrow(original)-3),rep("red",3))
colrs = c(colrsO,rep("blue",3),rep("green",3))

shaps0 = c(rep(20,nrow(original)-2),89,88)
shaps = c(shaps0,rep(c(20,89,88),2))

siz0 = c(rep(1,nrow(original)-3),rep(8,3))
siz =c(siz0,rep(8,6))

#Plot the shape ####
origPlot = ggplot(data = original, aes(x, y)) +  
  geom_vline(xintercept = 0, linetype = "longdash") + geom_hline(yintercept = 0, linetype = "longdash") +
  geom_point(shape = shaps0, colour = colrsO, size = siz0) + 
  scale_x_continuous(limits = c(-4,4)) + scale_y_continuous(limits = c(-4,4)) +
  theme(legend.position="none")
origPlot  #Note that the unit points on the X and Y axes are labeled with large red letters

#Transform the shape so that the axes are no longer the best basis ####
rotAngleDegrees = 30 #enter value in degrees
rotAng = pi*rotAngleDegrees/180
rotator = matrix(data = c(sin(rotAng),cos(rotAng),-cos(rotAng),sin(rotAng)), nrow = 2)
shift = c(.5,-.5)
#determinant(rotator, logarithm = F)
transformed = addMeanPoints(addKeyPoints(as.data.frame(
  x = (as.matrix(original) %*% rotator) + matrix(data = rep(shift, nrow(original)), ncol = 2, byrow = T) )))
colnames(transformed) = c("a","b") #name the new axes
transPlot = ggplot(data = transformed, aes(a, b)) +  
  geom_vline(xintercept = 0, linetype = "longdash") + geom_hline(yintercept = 0, linetype = "longdash") +
  geom_point(colour = colrs, shape = shaps, size = siz) + 
  scale_x_continuous(limits = c(-4,4)) + scale_y_continuous(limits = c(-4,4)) +
  theme(legend.position="none")
transPlot  #The red letters are the transformed image of the original unit points.  
           #The blue letters are the new unit points.  
           #The green letters are the unit points based on the center of mass of the plot

#Add a little noise to the plot so PCA has something to do ####
dataformed = transformed + data.frame(a = runif(nrow(transformed),-.02,.02), b = runif(nrow(transformed),-.02,.02))
colnames(dataformed) = c("a_ish", "b_ish")
meanA = mean(dataformed$a_ish)
meanB = mean(dataformed$b_ish)
dataPlot = ggplot(data = dataformed, aes(a_ish, b_ish)) + 
  geom_vline(xintercept = 0, linetype = "longdash") + geom_hline(yintercept = 0, linetype = "longdash") +
  geom_point(shape = shaps, colour = colrs, size = siz) + 
  scale_x_continuous(limits = c(-4,4)) + scale_y_continuous(limits = c(-4,4)) +
  theme(legend.position="none")
dataPlot


#Run PCA and plot the new basis vectors against the dataformed shape ####
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
      colour = "red"), 
    arrow = arrow()) +
  geom_segment(
    aes(
      x = center[1], 
      y = center[2], 
      xend = center[1] + pcaMatrix[1,2], 
      yend = center[2] + pcaMatrix[2,2],
      colour = "red"), 
    arrow = arrow())
pcaPlot
#Note that the new basis vectors point in the same directions as 
#vectors from the original origin (red dot) to the original unit points (red X and Y).

#Grab the "scores" from PCA and plot them ####
#This is the data reorganized into the new space
#Also provide the old basis vectors (rows of the loadings matrix)
scoresFromPCA = pcaOutput$scores
newDim = as.data.frame(scoresFromPCA)
newDimPlot = ggplot(data = newDim, aes(Comp.1, Comp.2)) + 
  geom_vline(xintercept = 0, linetype = "longdash") + geom_hline(yintercept = 0, linetype = "longdash") +
  geom_point(shape = shaps, colour = colrs, size = siz) + 
  scale_x_continuous(limits = c(-4,4)) + scale_y_continuous(limits = c(-4,4)) +
  theme(legend.position="none") +
  geom_segment(
    aes(
      x = 0, 
      y = 0, 
      xend = pcaMatrix[1,1], 
      yend = pcaMatrix[1,2]),
      colour = "green", 
    arrow = arrow()) +
  geom_segment(
    aes(
      x = 0, 
      y = 0, 
      xend = pcaMatrix[2,1], 
      yend = pcaMatrix[2,2]),
      colour = "green", 
    arrow = arrow())
newDimPlot
#Note that, if there was no translation, the old basis vectors terminate at unit points from the dataformed ellipse
#If there was a translation, the old basis vectors terminate at points 1 unit from the center of mass from the dataformed ellipse



# Compare PCA scores from the PCA output to calculated scores using the loading matrix ####
str(scoresFromPCA)
dataformed.centered = dataformed #copy the data
for(i in 1:ncol(dataformed)){ #center each column
  dataformed.centered[,i] = dataformed.centered[,i] - mean(dataformed.centered[,i])
}
colMeans(dataformed.centered) #check the centering
scoresFromMatrix = as.matrix(dataformed.centered) %*% pcaOutput$loadings[,] #calculate PCA scores

all(scoresFromMatrix == scoresFromPCA) #make sure they are all the same
