#functions.R

# Load libraries ####
library(ggplot2)
library(shapes)
library(grid)
library(mice)
library(scatterplot3d)


# addKeyPoints ####
# This adds the origin and the x and y axis unit points to a dataframe
# x - a dataframe with 2 columns representing the x and y dimensions and one row for each point
addKeyPoints = function(x){
  pts = data.frame(x = c(0,0,1), y = c(0,1,0))
  colnames(pts) = colnames(x)
  rbind.data.frame(x,pts)
}


# addMeanPoints ####
# This adds a point at the center of mass to to a dataframe
# x - a dataframe with 2 columns representing the x and y dimensions and one row for each point 
addMeanPoints = function(x){
  mX = mean(x[,1])
  mY = mean(x[,2])
  pts = data.frame(x = c(mX,mX,mX+1), y = c(mY,mY+1,mY))
  colnames(pts) = colnames(x)
  rbind.data.frame(x,pts)
} 


# pcaLoadingStack ####
# stacking the loading matrices from running pca after mice
# pca.output - mira object, output from doing PCA on a mids object
# transpose - logical, indicating whether to transpose the loading matrices
# origin - logical, indicating whether to add the origin point to the loading matrices
# vertical - character, indicating the prefix for names of the z-axis levels in the output
pcaLoadingStack = function(pca.output, transpose = TRUE, origin = TRUE, vertical = "Imputation"){
  b = lapply(pca.output$analyses, FUN = function(a){a$loadings[]}) #extract the loading matrices
  if(origin){  b = lapply(b, cbind, origin = rep(0,times = nrow(b[[1]]))) } #add the origin point
  if(transpose){ b = lapply(b, t) } #transpose
  x = array(data = unlist(b), dim = c(dim(b[[1]]), length(b))) #make the array
  newDimNames = dimnames(b[[1]]) #extract the old dimnames
  newDimNames[[3]] = paste0(vertical,1:length(b)) #add dimnames for the 3rd dimension
  dimnames(x) = newDimNames #assign the dimnames to the array
  return(x) #return it
}


# procPCA ####
# run PCA on a mids object and return the merged loading matrix
# d1.mi - mids object, output from mice()
# varNums - integer vector indicating the columns to be used in PCA
procPCA = function(d1.mi, varNums){
  varSet = names(complete(d1.mi)) #get the variables names
  varSet.pca = paste0(varSet[varNums],collapse = ",") #select only the variables to be used in pca
  pcaCall = paste0("with(d1.mi, princomp(cbind.data.frame(",varSet.pca,"),scores = T))") #build the call
  d1.mi.pca = eval(parse(text = pcaCall)) #run the call
  loadingstack = pcaLoadingStack(d1.mi.pca) #transpose the matrices, add an origin point, and stack them in an array
  d1.gpa = procGPA(loadingstack, reflect = T, proc.output = T, pcaoutput = F, scale = FALSE) #get the results of GPA
  mean.pca = d1.gpa$mshape #pull out the mean matrix
  dimnames(mean.pca) = dimnames(loadingstack)[-3] #set the dimnames on the mean matrix
  pca.pool = mean.pca - #readjust the matrix to move the origin back to 0 and the points back to the unit sphere
    matrix(rep(mean.pca[nrow(mean.pca),], times = nrow(mean.pca)), ncol = ncol(mean.pca), byrow = T)  
  basis = t(pca.pool[-nrow(pca.pool),])  #pull out the pooled pca loading matrix with origin point removed
  return(basis)
}


# addPCAscores ####
# append PCA scores to the data sets in a mids object
# d1.mi - mids object, output from mice()
# basis - PCA loadings matrix, obtainable from procPCA()
addPCAscores = function(d1.mi, basis){
  d1.mi.long = complete(d1.mi, action = "long", include = T) #extract all imputed data sets as one data.frame
  n = nrow(d1.mi.long)/(1 + d1.mi$m) #calculate the n of the original experiment
  scores = matrix(data = NA, nrow = 0, ncol = ncol(basis)) #set up an empty matrix to hold the scores
  for (i in 1:(d1.mi$m + 1)){ #for each data set,
    raw = d1.mi.long[(1000*(i-1)+1):(1000*i),c("x","y","z")] #extract it from the long form and keep only pca variables
    centered = as.matrix(raw) - matrix(rep(colMeans(raw, na.rm = T), nrow(raw)), byrow = T, ncol = ncol(raw)) #center the variables
    scores = rbind(scores, centered %*% basis) #calculate the scores and append them to the scores matrix
  }
  d1.mi.long.aug = cbind(d1.mi.long, scores) #attach the scores to the long form data
  d1.mi.aug = as.mids(d1.mi.long.aug) #convert it back to a mids object
  return(d1.mi.aug)
}

