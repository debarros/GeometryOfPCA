#procrustes.R

source("functions.R")

# Table of variable meanings for GPA ####
# | array dim | shape analysis    | pca merging              |
# |-----------|-------------------|--------------------------|
# |     k     | no. of points     | no. of PCA components    |
# |     m     | no. of dimensions | no. of variables         |
# |     n     | sample size       | no. of imputed data sets |

# Summary ####
# GPA is often used to take multiple sets of points defining a shape and transform them to match the shapes
# In PCA, each component is represented by a unit vector
# This unit vector terminates at a point with coordinates that correspond to the (centered) original variables
# Therefore, the PCA loading matrix can be thought of as a set of points.
# There is one point for each component, and as many dimensions as there are original variables.
# This set of points exists on the unit sphere in m dimensions, where m is the number of original variables.
# PCA applied to MI datasets will produce n sets of these points (where n is the number of imputations)
# This can be thought of n shapes that must be combined/merged
# GPA merges shapes defined by sets of points, but also recenters them
# Therefore, the resultant mean matrix output from GPA must translated to move the points back onto the unit sphere
# The easiest way to do this is to add the origin as a point to each of the PCA loading matrices
# Then, the coordinates of the image of the origin can be subtracted from each of the points in the mean matrix to yeild the basis matrix
# This moves the origin back to the origin, and the components back to the unit m-sphere, making the basis matrix a usable set of pca loadings
# To append component scores to the imputed data sets, 
#   Extract the datasets from the mids object in long form
#   Take each dataset from the long form, keeping only the pca variables
#   Center each of the variables in the subset
#   Multiply the centered subset by the basis matrix to get the scores matrix
#   compile a long form of the scores matrix (append by rows)
#   Append the scores matrix to the long form dataset (append by columns)
#   Use as.mids() to convert the augmented long form back into a mids object

# Set up the data ####
sampleSize = 1000
a = rnorm(sampleSize)
b = rnorm(sampleSize)
x1 = a + rnorm(sampleSize,0,.2)
y1 = a + b + rnorm(sampleSize,0,.2)
z1 = a/2 - b/2 + rnorm(sampleSize,0,.5)
type1 = rep("A", times = sampleSize)
type1[a>b] = "B"
type1[y1>z1] = "C"
type1 = factor(type1)
outcome1 = x1 + y1 + z1 + rnorm(sampleSize,0,.5) + as.numeric(type1)
d1 = data.frame(x = x1, OtherID = rep(letters, 39)[1:1000], y = y1, z = z1, RowID = c(1:1000), type = type1, outcome = outcome1)

# Apply missingness and impute data ####
missingnessRate = .3 
imputations = 3
iterations = 3
missingness = matrix(data = as.logical(rbinom(dim(d1)[1]*dim(d1)[2],1,missingnessRate)), ncol = dim(d1)[2])
colnames(missingness) = colnames(d1)
missingness[,c("OtherID","RowID")] = FALSE
d1.miss = d1
d1.miss[which(missingness, arr.ind = T)] = NA
for(i in 1:ncol(d1.miss)){  print(sum(is.na(d1.miss[,i]))) }
for(i in 1:nrow(d1.miss)){
  if(sum(is.na(d1.miss[i,c("x","y","z")])) < 3 ) {
    eliminator = sample(x = c("x","y","z"), size = 1)
    d1.miss[1,eliminator] = NA
  }
}


d1.mi = mice(d1.miss, maxit = 0)
pred = d1.mi$predictorMatrix
pred[, "RowID"] = 0
pred[, "OtherID"] = 0
d1.mi = mice(data = d1.miss, predictorMatrix = pred, m = imputations, maxit = iterations, printFlag = F)

# Run PCA on the imputed data sets and pool the results with GPA ####
varNums = c(1,3,4) #enter the column numbers to be used in PCA
basis = procPCA(d1.mi, varNums)


# Calculate the component scores and insert them into the mids object ####
d1.mi.aug = addPCAscores(d1.mi, basis)
sum(is.na(complete(d1.mi.aug,"long")))
d1.mi.longagain = complete(d1.mi.aug, "long", include = T)
sum(is.na(d1.mi.longagain))
sum(is.na(d1.miss))
d1.mi.long = complete(d1.mi, "long", include = F)
sum(is.na(d1.mi.long))
# Run the analysis on the new mids object ####

regmodels = with(d1.mi.aug, lm(outcome ~ Comp.1 + Comp.2 + Comp.3 + type))
pooled.model = pool(regmodels)
summary(pooled.model)
pooled.model

