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
x = a + rnorm(sampleSize,0,.2)
y = a + b + rnorm(sampleSize,0,.2)
z = a/2 - b/2 + rnorm(sampleSize,0,.5)
type = rep("A", times = sampleSize)
type[a>b] = "B"
type[y>z] = "C"
type = factor(type)
outcome = x + y + z + rnorm(sampleSize,0,.5) + as.numeric(type)
d1 = data.frame(x = x, y = y, z = z, type = type, outcome = outcome)

# Apply missingness and impute data ####
missingnessRate = .2 
imputations = 5
missingness = matrix(data = as.logical(rbinom(dim(d1)[1]*dim(d1)[2],1,missingnessRate)), ncol = dim(d1)[2])
d1.miss = d1
d1.miss[which(missingness, arr.ind = T)] = NA
d1.mi = mice(d1.miss, m = imputations, printFlag = F)

# Run PCA on the imputed data sets and pool the results with GPA ####
varNums = 1:3 #enter the column numbers to be used in PCA
basis = procPCA(d1.mi, varNums)


# Calculate the component scores and insert them into the mids object ####
d1.mi.aug = addPCAscores(d1.mi, basis)

# Run the analysis on the new mids object ####

regmodels = with(d1.mi.aug, lm(outcome ~ Comp.1 + Comp.2 + Comp.3 + type))
pooled.model = pool(regmodels)
summary(pooled.model)
pooled.model

