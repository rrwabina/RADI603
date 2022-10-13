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
library(keras)
library(neuralnet)
library(spatstat)
library(OpenImageR)
library(raster)
library(ROSE)

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
table(df_autism_bal$class)
df_autism <- ovun.sample(class ~ ., data = df_autism, method = 'over', N = 125)$data

######################################################################################################
#### CLASSIFIER: SVM Binary Classification
set.seed(224599)
df_autism[1:16] <- scale(df_autism[1:16])
idx <- sample(2, nrow(df_autism), replace = TRUE, prob=c(0.75, 0.25))
autism_train <- as.data.frame(df_autism[idx==1,])
autism_test  <- as.data.frame(df_autism[idx==2,])
tune  <- tune.svm(class~., data = autism_train, gamma = 10^(-6:-1), cost = 10^(1:4), tunecontrol = tune.control(cross = 5))
summary(tune)
set.seed(224599)
model <- svm(class~., data = autism_train, 
                      method = 'C-classification', 
                      kernel = 'polynomial', degree = 3, 
                      probability = T, 
                      gamma = 1e-02, 
                      cost = 1000)

prediction <- predict(model, autism_test, probability = T)
table(autism_test$class, prediction)
confusionMatrix(autism_test$class, prediction)
######################################################################################################
#### CLASSIFIER: KNN-AUTISM
idx <- sample(2, nrow(df_autism), replace = TRUE, prob=c(0.75, 0.25))
autism_train <- as.data.frame(df_autism[idx == 1,])
autism_test  <- as.data.frame(df_autism[idx == 2,])

# Did not use the results column
train_input  <- as.matrix(autism_train[, -16])
train_output <- as.matrix(autism_train[,  17])
test_input   <- as.matrix(autism_test[,  -16])

prediction <-knn(train_input, test_input, train_output, k = 3)
xtab <- table(factor(prediction, levels = c(0, 1)), factor(autism_test$class, levels = c(0, 1)))
results <- confusionMatrix(xtab)
results
as.matrix(results, what = "overall")
as.matrix(results, what = "classes")
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

cor(df_autism[, 1:16], as.numeric(df_autism[, 17]), method=c('pearson', 'kendall', 'spearman'))

nn <- neuralnet(NO + YES ~ 
                A3.Score + A4.Score + A5.Score + A6.Score  + 
                A7.Score + A8.Score + A9.Score + A10.Score + used,
                data = nnet_autismtrain, hidden=c(4))
plot(nn)

mypredict <- compute(nn, autism_test[-16])$net.result
maxidx <- function(arr){ 
  return(which(arr == max(arr)))
}

idx <- apply(mypredict, c(1), maxidx)
prediction <- c(0, 1)[idx]
u <- union(prediction, autism_test$class)
xtab <- table(factor(prediction, u), factor(autism_test$class, u))
results <- confusionMatrix(xtab)
as.matrix(results, what = 'overall')
as.matrix(results, what = 'classes')

roc.curve(autism_test$class, prediction, plotit = TRUE)

#################################################### LEUKEMIA DATA SVM ###########################################################
df_leukem <- read.csv('../data/leukemiaData.csv')

sapply(df_leukem, function(x) sum(is.na(x)))
sapply(df_leukem, function(x) typeof(x))
sapply(df_leukem, function(x) unique(x))
sapply(df_leukem, function(x) is.numeric(x))

df_leukem$Class = factor(df_leukem$Class,
                         levels = c('ALL', 'MLL', 'AML'),
                         labels = c(0, 1, 2))

for (idx in 1:(ncol(df_leukem)-1))
{
  df_leukem[, idx] <- as.numeric(df_leukem[, idx])
}
df_leukem[, 1:(ncol(df_leukem)-1)] <- scale(df_leukem[, 1:(ncol(df_leukem)-1)])

#### CLASSIFIER: SVM Multi-Classification
set.seed(224599)
idx <- sample(2, nrow(df_leukem), replace = TRUE, prob=c(0.7, 0.3))
leukem_train <- as.data.frame(df_leukem[idx==1,])
leukem_test  <- as.data.frame(df_leukem[idx==2,])
tune  <- tune.svm(Class~., data = leukem_train, gamma = 10^(-10:-1), cost = 10^(1:5), tunecontrol = tune.control(cross = 10))
summary(tune)

set.seed(224599)
model <- svm(Class~., data = leukem_train, 
             method = 'C-classification', 
             kernel = 'polynomial', degree = 5,
             probability = T, 
             gamma = 1e-01, 
             cost  = 1e+05)

prediction <- predict(model, leukem_test, probability = T)
table(leukem_test$Class, prediction)
confusionMatrix(leukem_test$Class, prediction)

#################################################### LEUKEMIA DATA KNN ###########################################################
idx <- sample(2, nrow(df_leukem), replace = TRUE, prob=c(0.6, 0.4))
leukem_train <- as.data.frame(df_leukem[idx==1,])
leukem_test  <- as.data.frame(df_leukem[idx==2,])
train_input  <- as.matrix(leukem_train[, -15])
train_output <- as.matrix(leukem_train[,  15])
test_input   <- as.matrix(leukem_test[,  -15])

help(knn)
prediction <- knn(train_input, test_input, train_output, k = 5)
xtab <- table(prediction, leukem_test$Class)
results <- confusionMatrix(xtab)
results
as.matrix(results, what = "overall")
as.matrix(results, what = "classes")

############################################# LEUKEMIA DATA ANN ###########################################################
set.seed(1000)
idx <- sample(2, nrow(df_leukem), replace = TRUE, prob=c(0.7, 0.3))
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
                data = nnet_leukemtrain, hidden = c(3),
                act.fct  = 'logistic', linear.output = FALSE)
help(neuralnet)
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
objective.in <- c(500, 200, 300, 800)
const.mat  <- matrix(c(400, 200, 150, 500, 3, 2, 0, 0, 2, 2, 4, 4, 2, 4, 1, 5), nrow = 4, byrow = TRUE)
const_cal  <-  500 
const_prot <- 6  # in grams
const_carb <- 10 # in grams
const_fat  <-  8  # in grams
const.rhs  <- c(const_cal, const_prot, const_carb, const_fat)
const.dir  <- c(">=", ">=", ">=", ">=")
opt <- lp(direction = "min", objective.in , const.mat, const.dir, const.rhs)
summary(opt)
opt$solution
opt$objval

## As we see for our optimum solution means diet with minimum cost 
## Ajarn should have 0 units of Food1, 3 units of Food2, 1 unit of Food3 and 0 units of Food4. 
## The cost of diet will be 900B and this diet will provide him at least 500 calories, 6 grams of protein, 10 grams of carbohydrates and 8 grams of fat.

################################################## IMAGE FILES AUGMENTATION #######################################################

xray1 <- image_read('../data/datatwo/x-ray01.jpg', density = NULL, depth = NULL, strip = FALSE)
image_info(xray1)
image_convert(xray1, format = 'png', depth = NULL)


image_trim(xray1)
image_crop(xray1,  '100x100')

image_scale(xray1, 'x500')
image_scale(xray1, 'x100')
image_scale(xray1, 'x50')

image_rotate(xray1, 45)
image_rotate(xray1, 90)

image_modulate(xray1, brightness = 20, saturation = 120, hue = 90)
image_modulate(xray1, brightness = 50, saturation = 120, hue = 90)

my_raster <- as.raster(xray1) 
par(mar = c(0, 0, 0, 0))
plot(my_raster[1:125, 1:102])


image_fill(xray1, 'blue', point = '+100+200', fuzz = 30)

image_border(image_background(xray1, 'hotpink'), "#000350", '10x10')
image_annotate(
  xray1, text = 'X-ray 1', size = 30, color = "blue",
  gravity = 'center')
image_annotate(
  xray1, text = ' X-ray 1 ', size = 30,
  color = 'red', boxcolor = 'white',
  degrees = 0, location = '+50+100',
  font = 'Arial')


datagen <- image_data_generator(
  rescale = 1/255,
  rotation_range = 40,
  width_shift_range = 0.2,
  height_shift_range = 0.2,
  shear_range = 0.2,
  zoom_range = 0.2,
  horizontal_flip = TRUE,
  fill_mode = "nearest"
)
xray1 <- image_read('../data/datatwo/x-ray01.jpg', density = NULL, depth = NULL, strip = FALSE)

img_path = '../data/datatwo/x-ray01.jpg'
image1 = readImage(img_path) 
library(tensorflow)
augmentation_generator <- flow_images_from_data(
  image1, 
  generator = datagen,
  batch_size = 1
)
