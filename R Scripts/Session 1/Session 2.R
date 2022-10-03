# Perceptron
colonData <- read.table("../data/colon.csv", header = TRUE, sep= ",")
x <- data.frame(colonData$H64807, colonData$T62947)
y <- colonData$Class
inweight1 <- rep(0, dim(x)[2] + 1)


perceptron <- function(x, y, inWeight, eta, niter) {
  weight <- inWeight
  errors <- rep(0, niter)
  
  for (jj in 1:niter) {
    for (ii in 1:length(y)) {
      z <- sum(weight[2:length(weight)] *
                 as.numeric(x[ii, ])) + weight[1]
      if(z < 0) {
        ypred <- -1
        } 
      else {
        ypred <-1
        }
      weightdiff <- eta * (y[ii] - ypred) * c(1, as.numeric(x[ii, ]))
      weight <- weight + weightdiff
      if ((y[ii] - ypred) != 0.0) {
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
