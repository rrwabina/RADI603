
################################################################################ Perceptron
colonData <- read.table("../data/colon.csv", header = TRUE, sep= ",")
x <- data.frame(colonData$H64807, colonData$T62947)
y <- colonData$Class

# No training and testing data?

inweight1 <- rep(0, dim(x)[2] + 1)

perceptron <- function(x, y, inWeight, eta, niter) {
  weight <- inWeight
  errors <- rep(0, niter) 
  
  # loop over number of iterations/epochs
  for (jj in 1:niter) {           
    # loop through the training set
    for (ii in 1:length(y)) {
      
      # Heaviside activation function
      # Binary labels
      z <- sum(weight[2:length(weight)] * as.numeric(x[ii, ])) + weight[1]
      
      if(z < 0) {
        ypred <- -1
        } 
      else {
        ypred <-  1
        }
      weightdiff <- eta * (y[ii] - ypred) * c(1, as.numeric(x[ii, ])) # Loss function
      weight <- weight + weightdiff
      if ((y[ii] - ypred) != 0.0) { # Update error function
        errors[jj] <-errors[jj] + 1
      }
    }
  }
  print(weight)
  return(errors)
}

err1 <- perceptron(x, y, inweight1, 0.0001, 50)


plot(1:50, err1, type="l", lwd=2, col = "red", xlab = "epoch #", ylab = "errors")
title("Errors vs epoch - learning rate eta = 1")


################################################################################ Support Vector Machines

library(e1071)
library(MASS)
data(cats)

catstrain <- cats
catstest  <- cats
tune <- tune.svm(Sex~., data=catstrain, gamma=10^(-6:-1), cost=10^(1:4))

model <- svm(Sex~., data=catstrain, method="C-classification", kernel="linear", probability=T, gamma=0.1, cost=10)
prediction <-predict(model, catstest, probability=T)
table(catstest$Sex, prediction)
plot(model, cats)

tune <- tune.svm(Sex~., data=catstrain, gamma=10^(-6:-1), cost=10^(1:4), tunecontrol=tune.control(cross=5))
summary(tune)

help('tune.svm')




library(e1071)
library(MASS)
data(cats)
catsData <-cats
set.seed(224599)
ind <- sample(2, nrow(catsData), replace=TRUE, prob=c(0.7, 0.3))
catstrain <- catsData[ind==1,]
catstest  <- catsData[ind==2,]
tune  <- tune.svm(Sex~., data=catstrain, gamma=10^(-6:-1), cost=10^(1:4))
summary(tune)
model <- svm(Sex~., data=catstrain, method="C-classification", kernel="linear", probability=T, gamma=0.1, cost=10)
prediction <- predict(model, catstest, probability=T)
table(catstest$Sex, prediction)
plot(model, cats)


library(caret)
# confusionMatrix(catstrain$Sex, predict(model))

set.seed(101)
#get samples from iris data
sampleiris <- iris[sample(1:150, 40),]
#each observation has 4 variables, ie. They are interpreted as 4-Dimension
distance <-dist(sampleiris[,-5], method="euclidean")
cluster <-hclust(distance, method="average")
plot(cluster, hang=-1, label=sampleiris$Species)

#prune the tree by 3 cluster
group.3 <-cutree(cluster, k=3)
#compare with known classes
table(group.3, sampleiris$Species)

plot(sampleiris[,c(1,2)], col=group.3, pch=19, cex=2.5, main="3 clusters")
points(sampleiris[,c(1,2)], col=sampleiris$Species, pch=19, cex=1)




library(neuralnet)
irisNN <- iris
set.seed(152)
ind <- sample(2, nrow(irisNN), replace=TRUE, prob=c(0.7, 0.3))
trainData <- irisNN[ind==1,]
testData <- irisNN[ind==2,]
nnet_iristrain <- trainData

#binarize the categorical output
nnet_iristrain <- cbind(nnet_iristrain, trainData$Species == 'setosa')
nnet_iristrain <- cbind(nnet_iristrain, trainData$Species == 'versicolor')
nnet_iristrain <- cbind(nnet_iristrain, trainData$Species == 'virginica')
names(nnet_iristrain)[6:8] <- c('setosa', 'versicolor', 'virginica')

nn <- neuralnet(setosa+versicolor+virginica ~
                  Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,
                  data=nnet_iristrain, hidden=c(3))

plot(nn)
mypredict <- compute(nn, testData[-5])$net.result

#put multiple binary output to categorical output
maxidx <- function(arr){ return(which(arr == max(arr)))}
idx <- apply(mypredict, c(1), maxidx)
prediction <- c('Iris-setosa', 'Iris-versicolor', 'Iris-virginica')[idx]
table(prediction, testData$Species)




autism <- read.csv('../data/Autism-Adolescent-Data.csv')
leukem <- read.csv('../data/leukemiaData.csv')
library(ggplot2)
library(GGally)
sapply(autism, function(x) sum(is.na(x)))
sapply(leukem, function(x) sum(is.na(x)))

sapply(autism, function(x) typeof(x))
View(autism)

library(dplyr)
autism_preprocessed <- autism %>%
  mutate(A3.Score = as.numeric(A3.Score))

remove.packages("rlang")
remove.packages("dplyr")

install.packages("rlang")
install.packages("dplyr")

library(rlang)
library(dplyr)
install.packages('lifecycle')
install.packages('pillar')