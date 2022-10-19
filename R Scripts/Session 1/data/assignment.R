library(ggplot2)
library(GGally)
library(dplyr)
library(naniar)
library(e1071)
library(MASS)
library(caret)
library(class)
library(lpSolve)
library(magick)
library(tensorflow)
library(neuralnet)
library(spatstat)
library(OpenImageR)
library(raster)
library(ROSE)
library(SpatialPack)
library(devtools)
library(tibble)

setwd('C:/Users/Renan/Desktop/PhD Data Science/RADI 603/R Scripts/Session 1/data')
df_autism <- read.csv('../data/Autism-Adolescent-Data.csv')
colnames(df_autism)[13] <- 'jaundice'
colnames(df_autism)[14] <- 'autism'
colnames(df_autism)[15] <- 'used'
colnames(df_autism)[17] <- 'class'

# Check the data
sapply(df_autism, function(x) sum(is.na(x)))
sapply(df_autism, function(x) typeof(x))
sapply(df_autism, function(x) unique(x))
sapply(df_autism, function(x) is.numeric(x))
sapply(df_autism, function(x) class(x))

# Encoding the categorical data
df_autism$gender = factor(df_autism$gender,
                          levels = c('m', 'f'),
                          labels = c(1, 0))

df_autism$jaundice = factor(df_autism$jaundice,
                            levels = c('yes', 'no'),
                            labels = c(1, 0))

df_autism$autism = factor(df_autism$autism,
                          levels = c('yes', 'no'),
                          labels = c(1, 0))

df_autism$used = factor(df_autism$used,
                        levels = c('yes', 'no'),
                        labels = c(1, 0))

df_autism$class = factor(df_autism$class,
                         levels = c('YES', 'NO'),
                         labels = c(1, 0))



# Replace vague values as NA
df_autism <- df_autism %>% 
  replace_with_na(replace = list(A3.Score  = "?")) %>%
  replace_with_na(replace = list(A6.Score  = "?")) %>%
  replace_with_na(replace = list(A10.Score = "O"))

# Convert string to integer
df_autism$A3.Score  <- as.integer(df_autism$A3.Score)
df_autism$A6.Score  <- as.integer(df_autism$A6.Score)
df_autism$A10.Score <- as.integer(df_autism$A10.Score)


# Imputation
df_autism$A3.Score[is.na(df_autism$A3.Score)]   <- median(df_autism$A3.Score,  na.rm = TRUE)
df_autism$A6.Score[is.na(df_autism$A6.Score)]   <- median(df_autism$A6.Score,  na.rm = TRUE)
df_autism$A10.Score[is.na(df_autism$A10.Score)] <- median(df_autism$A10.Score, na.rm = TRUE)


# Scaling/Normalization
for (idx in 1:16)
  {
    df_autism[, idx] <- as.numeric(df_autism[, idx])
  }

########################## Please delete if oversampling is not required #############################
# Check histogram
hist(as.numeric(df_autism$class))
prop.table(table(df_autism$class))
table(df_autism$class)
df_autism <- ovun.sample(class ~ ., data = df_autism, method = 'over', N = 125)$data

######################################################################################################
#### CLASSIFIER: SVM Binary Classification
# I added manual seeding in data splitting to provide the same results when running the model
# Users may opt to delete the seeding
set.seed(224599) 
df_autism[1:16] <- scale(df_autism[1:16])
idx <- sample(2, nrow(df_autism), replace = TRUE, prob=c(0.75, 0.25))
autism_train <- as.data.frame(df_autism[idx==1,])
autism_test  <- as.data.frame(df_autism[idx==2,])
tune  <- tune.svm(class~., data = autism_train, gamma = 10^(-6:-1), cost = 10^(1:4), tunecontrol = tune.control(cross = 10))

set.seed(224599)
# Building the SVM classifier
model <- svm(class~., data = autism_train, 
             method = 'C-classification', 
             kernel = 'polynomial', degree = 3, 
             probability = T, 
             gamma = 1e-02, 
             cost = 1000)

prediction <- predict(model, autism_test, probability = T)
# table(autism_test$class, prediction)
confusionMatrix(autism_test$class, prediction)
######################################################################################################
#### CLASSIFIER: KNN-AUTISM
set.seed(10000)
idx <- sample(2, nrow(df_autism), replace = TRUE, prob=c(0.75, 0.35))
autism_train <- as.data.frame(df_autism[idx == 1,])
autism_test  <- as.data.frame(df_autism[idx == 2,])

# Did not use the results column
train_input  <- as.matrix(autism_train[, -16])
train_output <- as.matrix(autism_train[,  17])
test_input   <- as.matrix(autism_test[,  -16])

# Building the KNN classifier
set.seed(10000)
prediction <-knn(train_input, test_input, train_output, k = 3)
xtab <- table(factor(prediction, levels = c(0, 1)), factor(autism_test$class, levels = c(0, 1)))
results <- confusionMatrix(xtab)
results

# You may uncomment the following scripts to see other performance indicators
# as.matrix(results, what = "overall")
# as.matrix(results, what = "classes")
######################################################################################################
#### CLASSIFIER: ANN-AUTISM

set.seed(152)
idx <- sample(2, nrow(df_autism), replace = TRUE, prob=c(0.7, 0.3))
autism_train <- as.data.frame(df_autism[idx == 1,])
autism_test  <- as.data.frame(df_autism[idx == 2,])
nnet_autismtrain <- autism_train

nnet_autismtrain <- cbind(nnet_autismtrain, autism_train$class == 0)
nnet_autismtrain <- cbind(nnet_autismtrain, autism_train$class == 1)
names(nnet_autismtrain)[18:19] <- c('NO', 'YES')

# The script cor(df) performs correlation matrix in the given dataset
# cor(df_autism[, 1:16], as.numeric(df_autism[, 17]), method=c('pearson', 'kendall', 'spearman'))

# Building the ANN classifier
nn <- neuralnet(NO + YES ~ 
                  A3.Score + A4.Score + A5.Score + A6.Score  + 
                  A7.Score + A8.Score + A9.Score + A10.Score + used,
                data = nnet_autismtrain, hidden=c(4))

# You may uncomment the following code to view neural net plot
# plot(nn)

mypredict <- compute(nn, autism_test[-16])$net.result
maxidx <- function(arr){ 
  return(which(arr == max(arr)))
}

idx <- apply(mypredict, c(1), maxidx)
prediction <- c(0, 1)[idx]
u <- union(prediction, autism_test$class)
xtab <- table(factor(prediction, u), factor(autism_test$class, u))
results <- confusionMatrix(xtab)
results

# You may uncomment the following scripts to see other performance indicators
# as.matrix(results, what = 'overall')
# as.matrix(results, what = 'classes')

# You may uncomment the following script to see the ROC curve
# roc.curve(autism_test$class, prediction, plotit = TRUE)
#################################################### LEUKEMIA DATA PROCESS ###################################################
df_leukem <- read.csv('../data/leukemiaData.csv')


df_leukem$Class = factor(df_leukem$Class, levels = c('ALL', 'MLL', 'AML'), labels = c(0, 1, 2))

for (idx in 1:(ncol(df_leukem)-1))
{df_leukem[, idx] <- as.numeric(df_leukem[, idx])}
df_leukem[, 1:(ncol(df_leukem)-1)] <- scale(df_leukem[, 1:(ncol(df_leukem)-1)], center = TRUE, scale  = TRUE)

# Check correlation between features and target
correlation <- cor(df_leukem[, 1:14], as.numeric(df_leukem[, 15]), method=c('pearson', 'kendall', 'spearman'))
#################################################### LEUKEMIA DATA SVM ###########################################################
set.seed(3407)
idx <- sample(2, nrow(df_leukem), replace = TRUE, prob=c(0.7, 0.3))


df_leukem_svm <- df_leukem[c('g4', 'g5', 'g7', 'g8', 'g10', 'g12', 'Class')]
leukem_train <- as.data.frame(df_leukem_svm[idx==1,])
leukem_test  <- as.data.frame(df_leukem_svm[idx==2,])
tune  <- tune.svm(Class~., data = leukem_train, 
                  gamma = 10^(-10:-1), 
                  cost  = 10^(1:5), 
                  tunecontrol = tune.control(cross = 10))


summary(tune)

set.seed(3407)
model <- svm(Class~., data = leukem_train, 
             method = 'C-classification', scale = FALSE,
             kernel = 'radial',
             probability = T, 
             gamma = 1e-03, 
             cost  = 1e+01)

prediction <- predict(model, leukem_test, probability = T)
xtab <- table(leukem_test$Class, prediction)
confusionMatrix(xtab)


#################################################### LEUKEMIA DATA KNN ###########################################################
set.seed(3407)
idx <- sample(2, nrow(df_leukem), replace = TRUE, prob = c(0.70, 0.30))

df_leukem_knn <- df_leukem[c('g4', 'g5', 'g7', 'g8', 'g10', 'g12', 'Class')]

leukem_train <- as.data.frame(df_leukem_knn[idx == 1,])
leukem_test  <- as.data.frame(df_leukem_knn[idx == 2,])
train_input  <- as.matrix(leukem_train[, -7])
train_output <- as.matrix(leukem_train[,  7])
test_input   <- as.matrix(leukem_test[,  -7])

set.seed(3407)
prediction <- knn(train_input, test_input, train_output, k = 29)
xtab <- table(prediction, leukem_test$Class)
results <- confusionMatrix(xtab)
results


as.matrix(results, what = 'overall')
as.matrix(results, what = 'classes')

roc.curve(factor(leukem_test$Class, levels = c(0, 1)), 
           factor(prediction, levels = c(0, 1)), plotit = TRUE)
############################################# LEUKEMIA DATA ANN ###########################################################
set.seed(1000)
idx <- sample(2, nrow(df_leukem), replace = TRUE, prob = c(0.7, 0.3))
leukem_train <- as.data.frame(df_leukem[idx==1,])
leukem_test  <- as.data.frame(df_leukem[idx==2,])
nnet_leukemtrain <- leukem_train

nnet_leukemtrain <- cbind(nnet_leukemtrain, leukem_train$Class == 0)
nnet_leukemtrain <- cbind(nnet_leukemtrain, leukem_train$Class == 1)
nnet_leukemtrain <- cbind(nnet_leukemtrain, leukem_train$Class == 2)

names(nnet_leukemtrain)[16:18] <- c('ALL', 'MLL', 'AML')

cor(df_leukem[, 1:14], as.numeric(df_leukem[, 15]), method=c('pearson', 'kendall', 'spearman'))

set.seed(1000)
nn <- neuralnet(ALL + MLL + AML ~ 
                  g4  + g5  + g7 + g8 + g10 + g12,
                data = nnet_leukemtrain, hidden = c(3), algorithm = 'rprop+',
                act.fct  = 'logistic', linear.output = FALSE)

mypredict <- compute(nn, leukem_test[-14])$net.result
maxidx <- function(arr){ 
  return(which(arr == max(arr)))
}
idx <- apply(mypredict, c(1), maxidx)
prediction <- c(0, 1, 2)[idx]
u <- union(prediction, leukem_test$Class)
xtab <- table(factor(prediction, u), factor(leukem_test$Class, u))
results <- confusionMatrix(xtab)
results

as.matrix(results, what = 'overall')
as.matrix(results, what = 'classes')

#################################################### LINEAR PROGRAMMING ###########################################################
f.objective   <- c(500, 200, 300, 800)
f.constraints <- matrix(c(  400, 200, 150, 500, 
                            3,   2,   0,   0, 
                            2,   2,   4,   4, 
                            2,   4,   1,   5), nrow = 4, byrow = TRUE)

calories      <- 500 
protein       <- 6  
carbohydrates <- 10 
fat           <- 8  

# Right-hand side of the LP constraints
f.rhs         <- c(calories, protein, carbohydrates, fat)
f.dir         <- c('>=', '>=', '>=', '>=')
optimal       <- lp(direction = 'min', f.objective, f.constraints, f.dir , f.rhs)

# summary(opt)
optimal$solution
optimal$objval

################################################## IMAGE FILES AUGMENTATION #######################################################

get_width <- function(image) {
  width  <- image_info(image)[2]
  width  <- width %>% getElement('width')
  return (width)
}

get_height <- function(image) {
  height <- image_info(image)[3]
  height <- height %>% getElement('height')
  return (height)
}

crop_image <- function(image, location, idx) 
{
  randomA = sample(100:200, 1)
  randomB = sample(100:200, 1)
  image <- image_crop(image, paste(as.character(randomA),
                                   'x',
                                   as.character(randomB), 
                                   sep = ''))
  loc = location
  path = paste(as.character(idx), 'crop.png', sep = '')
  png(file   = paste(loc, path),
      width  = get_width(image) + 50, 
      height = get_height(image)+ 50, 
      units = 'px', bg = 'transparent')
  plot(image)
  dev.off()
}


scale_image <- function(image, location, idx) 
{
  image <- image_scale(image, sample(200:500, 1))
  loc  = location 
  path = paste(as.character(idx), 'scale.png', sep = '')
  png(file   = paste(loc, path), 
      width  = get_width(image), 
      height = get_height(image), 
      units  = 'px', bg = 'transparent')
  plot(image)
  dev.off()
}


rotate_image <- function(image, location, idx) { 
  image <- image_rotate(image, sample(0:180, 1))
  loc  = location 
  path = paste(as.character(idx), 'rotate.png', sep = '')
  png(file = paste(loc, path), 
      width  = get_width(image), 
      height = get_height(image), 
      units = 'px', bg = 'transparent')
  plot(image)
  dev.off()
}


modulate_image <- function(image, location, idx) 
{
  image <- image_modulate(image, brightness = sample(5:200, 1), 
                          saturation = sample(1:100, 1), 
                          hue = sample(1:100, 1))
  loc  = location
  path = paste(as.character(idx), 'modulate.png', sep = '')
  png(file = paste(loc, path), 
      width  = get_width(image), 
      height = get_height(image), 
      units = 'px', bg = 'transparent')
  plot(image)
  dev.off()
}

colorize_image <- function(image, location, idx)
{
  image <- image_colorize(image, opacity = sample(10:50, 1),
                          color = sample(c('blue', 'red', 'yellow', 'green'), 1))
  loc  = location
  path = paste(as.character(idx), 'colorize.png', sep = '')
  png(file   = paste(loc, path), 
      width  = get_width(image), 
      height = get_height(image), 
      units = 'px', bg = 'transparent')
  plot(image)
  dev.off()
}

contrast_image <- function(image, location, idx) 
{
  image <- image_contrast(image, sharpen = sample(1:200, 1))
  loc  = location
  path = paste(as.character(idx), 'contrast.png', sep = '')
  png(file   = paste(loc, path), 
      width  = get_width(image), 
      height = get_height(image), 
      units = 'px', bg = 'transparent')
  plot(image)
  dev.off()
}

gaussianblur_image <- function(image, location, idx)
{
  image <- image %>% image_convolve('Gaussian:0x5', scaling = '70, 30%')
  loc  = location
  path = paste(as.character(idx), 'gaussian.png', sep = '')
  png(file   = paste(loc, path), 
      width  = get_width(image), 
      height = get_height(image), 
      units = 'px', bg = 'transparent')
  plot(image)
  dev.off()
}

edge_image <- function(image, location, idx)
{
  image <- image %>% image_convolve('Sobel') %>% image_negate()
  loc  = location
  path = paste(as.character(idx), 'edge.png', sep = '')
  png(file   = paste(loc, path), 
      width  = get_width(image), 
      height = get_height(image), 
      units = 'px', bg = 'transparent')
  plot(image)
  dev.off()
}

sobelscale_image <- function(image, location, idx)
{
  image <- image %>% image_convolve('Sobel', 
                                    scaling = sample(0:5, 1), 
                                    bias = sample(0:5, 1))
  loc  = location
  path = paste(as.character(idx), 'sobelscale.png', sep = '')
  png(file   = paste(loc, path), 
      width  = get_width(image), 
      height = get_height(image), 
      units = 'px', bg = 'transparent')
  plot(image)
  dev.off()
}

medianblur_image <- function(image, location, idx)
{
  image <- image_median(image, radius = sample(1:10, 1))
  loc  = location
  path = paste(as.character(idx), 'medianblur.png', sep = '')
  png(file   = paste(loc, path), 
      width  = get_width(image), 
      height = get_height(image), 
      units = 'px', bg = 'transparent')
  plot(image)
  dev.off()
}

negate_image <- function(image, location, idx)
{
  image <- image %>% image_convolve('Sobel') %>% image_negate()
  loc  = location
  path = paste(as.character(idx), 'negate.png', sep = '')
  png(file   = paste(loc, path), 
      width  = get_width(image), 
      height = get_height(image), 
      units = 'px', bg = 'transparent')
  plot(image)
  dev.off()
}

main <- function(image, location) {
  for (idx in 1:2)
  {
    scale_image(image, location, idx)
    rotate_image(image, location, idx)
    crop_image(image, location, idx)
    modulate_image(image, location, idx)
    colorize_image(image, location, idx)
    contrast_image(image, location, idx)
    gaussianblur_image(image, location, idx)
    edge_image(image, location, idx)
    sobelscale_image(image, location, idx)
    medianblur_image(image, location, idx)
    negate_image(image, location, idx)
  }
}

image <- image_read('../data/datatwo/x-ray01.jpg', density = NULL, depth = NULL, strip = FALSE)
location = '../data/xrays/xray1/'
main(image, location)

image <- image_read('../data/datatwo/x-ray02.jpg', density = NULL, depth = NULL, strip = FALSE)
location = '../data/xrays/xray2/'
main(image, location)

image <- image_read('../data/datatwo/x-ray03.jpg', density = NULL, depth = NULL, strip = FALSE)
location = '../data/xrays/xray3/'
main(image, location)
