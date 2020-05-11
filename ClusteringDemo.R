#library checker 

libs = c("matrixcalc", "ggplot2","fpc", "dbscan", "factoextra", "fpc", "MASS", "mclust")

for (i in libs){
  if( !is.element(i, .packages(all.available = TRUE)) ) {
    install.packages(i)
  }
  library(i,character.only = TRUE)
}
lapply(libs, require, character.only = TRUE)

# Set working directory to the folder where I saved my code 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#clear memory
rm(list=ls())

# sythetic data generation for demo
mu = c(2,3)  #we will create two groups/clusters with two different means
sigma=matrix(c(15,11,11,9),2,2) # covariance matrix
sigma

#the line below checks if the covariance matrix is a valid one 
#it checks if the matrix is a positive semi definite matrix
is.positive.semi.definite(sigma, tol=1e-8)
variables = mvrnorm(1000,mu,sigma)
plot(variables)
cor(variables)
cov2cor(sigma)  #this is the correlation matrix, if you are curious

#now, lets attempt to cluster

set.seed(20)
variables=as.data.frame(variables)
# scale(variables, scale=T)

clusterCenter = kmeans(variables, 2, nstart = 50)


clusterCenter

clusterCenter$cluster <- as.factor(clusterCenter$cluster)
dVariables=as.data.frame(variables) #in order to use ggplot, we need them in a certain format

ggplot(dVariables, aes(V1, V2, color = clusterCenter$cluster)) + geom_point()


# dev.off()
# plot(dVariables[,1], dVariables[,2], main="Scatter", col="blue", xlab="V1", ylab="V2", pch=24)
# abline(lm(dVariables[,2]~dVariables[,1]), col="red") # regression line (y~x) 






#now lets do it for our height and weight data
#height, weight, set means
muMale=c(185, 80)
sigmaMale=matrix(c(25,40,40,90),2,2)

sigmaMale
is.positive.semi.definite(sigmaMale, tol=1e-8)
mSubjects = mvrnorm(1000,muMale,sigmaMale)
dev.off()
plot(mSubjects)
cor(mSubjects)
cov2cor(sigmaMale)  #this is the correlation matrix, if you are curious

#now create female subjects :)
muFemale=c(175, 50)
sigmaFemale=matrix(c(20,35,35,80),2,2)

sigmaFemale
is.positive.semi.definite(sigmaFemale, tol=1e-8)
fSubjects = mvrnorm(1000,muFemale,sigmaFemale)
plot(fSubjects, col="pink")
cor(fSubjects)
cov2cor(sigmaFemale)  #this is the correlation matrix, if you are curious

#lets combine
fSubjects=as.data.frame(fSubjects)
fSubjects$gender="f"
mSubjects=as.data.frame(mSubjects)
mSubjects$gender="m"
str(fSubjects)
str(mSubjects)
Subjects=rbind(fSubjects,mSubjects)
names(Subjects)=c("Height","Weight", "Gender")
str(Subjects)
head(Subjects)
ggplot(Subjects, aes(Height, Weight, color = Gender)) + geom_point()

#now, lets attempt to do clustering

set.seed(20)
clusterCenter = kmeans(Subjects[,1:2], 2, nstart = 20)
clusterCenter

clusterCenter$cluster <- as.factor(clusterCenter$cluster)
ggplot(Subjects, aes(Height, Weight, color = clusterCenter$cluster)) + geom_point()


set.seed(123)
# DBSCAN: epsilon ("eps") and minimum points ("MinPts"). 
# The parameter eps defines the radius of neighborhood around a point x. 
# It's called called the \(\epsilon\)-neighborhood of x. 
# The parameter MinPts is the minimum number of neighbors within "eps" radius.


data("multishapes", package = "factoextra") #loading a sample data. 
df = multishapes[, 1:2]
head(multishapes)
plot(multishapes[,1:2])
multishapes$shape #4 different shapes are in the dataset. 

dbscan::kNNdistplot(df, k =  5)
abline(h = 0.15, lty = 2)
# It can be seen that the optimal eps value is around a distance of 0.15.

# Compute DBSCAN using fpc package
set.seed(123)
db = fpc::dbscan(df, eps = 0.15, MinPts = 5)
table(db$cluster)
df[db$cluster==0,]
# Plot DBSCAN results
plot(db, df, main = "DBSCAN", frame = FALSE)

# hmm.. can dbscan do our job?  let's give it a try 
db = fpc::dbscan(Subjects[,1:2], eps = .5, MinPts = 5)
# Plot DBSCAN results
plot(db, Subjects[,1:2], main = "DBSCAN", frame = FALSE)
print(db)


db = fpc::dbscan(Subjects[,1:2], eps = .5, MinPts = 2)
# Plot DBSCAN results
plot(db, Subjects[,1:2], main = "DBSCAN", frame = FALSE)

dbscan::kNNdistplot(Subjects[,1:2], k =  5)
abline(h = 1.6, lty = 2)
# It can be seen that the optimal eps value is around a distance of 1.6.
db = fpc::dbscan(Subjects[,1:2], eps = 2, MinPts = 2)
# Plot DBSCAN results
plot(db, Subjects[,1:2], main = "DBSCAN", frame = FALSE)


# it seems not!.. then 
# try dbscan with
# muMale=c(185, 90)
# sigmaMale=matrix(c(50,22,22,10),2,2)
# muFemale=c(185, 75)
# sigmaFemale=matrix(c(20,40,40,80),2,2)


# then one last try. 
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5096736/
# Mclust uses Bayesian Information Criterion (BIC) to find the number of
# clusters (model selection). BIC uses the likelihood and a penalty term to
# guard against overfitting.


str(Subjects)
mod = Mclust(as.matrix(Subjects[,1:2])) # this has no python implementation i think
mod$classification
mod$modelName
clustered=Subjects
clustered$cluster=mod$classification
clustered
mod$modelName
?Mclust
ggplot(clustered, aes(Height, Weight, color = cluster)) + geom_point()


#how many clusters will be needed? 
# Look for a bend or elbow in the sum of squared error (SSE) scree plot. 
# See http://www.statmethods.net/advstats/cluster.html & http://www.mattpeeples.net/kmeans.html 
# for more. 

mydata = clustered[,1:2]
wss = (nrow(mydata)-1) * sum(apply(mydata,2,var))
for (i in 1:15) wss[i] <- sum(kmeans(mydata,centers=i)$withinss)

plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

