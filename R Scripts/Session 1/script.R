x <- 5
y <- c(5, 9, 3)
meany = mean(y)
print(meany)

library(help = 'stats')


a <- c('Kolkata','Rome','New York', 'London', 'Melbourne')
b <- c(2,3,4,5)
c <- matrix(list(1,"a", 'b', 1+2i, TRUE, FALSE), 3, 3)
d <- list(2, 'London', 'YE')
print(a)
print(b)
print(c)
print(d)

for (x in d) {
  print(typeof(x))
}


emp.data <-data.frame(
  emp_id = c(1:5),
  emp_name = c("Rick","Dan","Michelle","Ryan","Gary"),
  salary = c(623.3,515.2,611.0,729.0,843.25),
  
  start_date = as.Date(c("2012-01-01", "2013-09-23", "2014-11-15", "2014-05-11",
                         "2015-03-27")),
  stringsAsFactors = FALSE
)
# Print the data frame.
print(emp.data)
df1 <- subset(emp.data, select = emp_id)
a <- emp.data[1]
print(a)
b <- emp.data[1, ]
print(b)
c <- emp.data[3, ]
print(c)
d <- emp.data[, 1]
print(d)

list_data <- list("Red", "Green", c(21,32,11), TRUE, 51.23, 119.1)


for (x in d) {
  print(typeof(x))
}

emp.data1 <- data.frame(
  emp_id = c(1:5),
  emp_name = c('Rick', 'Dan', 'Michelle', 'Ryan', 'Gary'),
  salary = c(623.3, 515.2, 611.0, 729.0, 843.25),
  start_date = as.Date(c("2012-01-01", "2013-09-23", "2014-11-15", "2014-05-11",
                         "2015-03-27")),
  stringsAsFactors = FALSE
)


fahrenheit_to_celsius <- function(temp_F) {
  temp_C <- (temp_F-32) * 5 / 9
  temp_C <- round(temp_C, 2)
  return(temp_C)
}
fahrenheit_to_celsius(90)

celsius_to_fahrenheit <- function(temp_C) {
  temp_F <- (temp_C) * (9 / 5) + 32
  temp_F <- round(temp_F, 2)
  return(temp_F)
}


dataCar = cars
carReg <- lm(speed~dist, data=dataCar)

aa <- emp.data[, 1:2]
bb <- emp.data[1:2, ]

# Statistical Functions in R
# Descriptive statistics --> summary(), stem()

irisData <- iris
summary(irisData)
plot(irisData$Sepal.Length, irisData$Sepal.Width)
print(table(iris$Species))

dataCar = cars
carReg <- lm(speed~dist, data=dataCar)
summary(carReg)
coef(carReg)

hist(dataCar$speed, prob = T)
lines(density(dataCar$speed))


library(foreign)
library(readxl)
colonData <- read.table('../data/colon.csv', header = TRUE, sep = ',')
colonExcelData <- read_excel('../data/colon.xlsx', sheet = 'colon')
colonData$Class <- factor(colonData$Class)
colonData$Class <-as.numeric(colonData$Class)

write.csv(colonData,'../data/colonTestSave.csv', row.names = FALSE)
library(writexl)
write_xlsx(list(conlonSheet = colonData),"../data/colonTestSaveExcel.xlsx")
write_xlsx(list(irisSheet = iris, carsSheet = cars, mtcarsSheet = mtcars), "../data/mydata.xlsx")


myPlot <- data.frame(
  x1 = c(175,166,181,168,177),
  x2 = c(65.5,45.3,95.01,65,80),
  stringsAsFactors = FALSE
)
plot(myPlot)


hist(colonData$H64808, prob=T)
lines(density(colonData$H64807))


categories <- table(colonData$Class)
barplot(categories, col=c('red','green'))

boxplot(H64807~Class, data = colonData)

library(ggplot2)
p <- ggplot(colonData, aes(colonData$H64807, colonData$T62947)) + 
     geom_point(aes(colour = factor(colonData$Class)))
p + scale_colour_manual(values = c("red", "green"))

library(ggplot2)
library(GGally)
ggpairs(iris, title = "Scatterplot Matrix of the Features of the Iris Data Set")


# Missing completely at random - the presence of missing values would not depend on the characteristics of the patient
# Missing at random - depends on the characteristics of the observable data

set.seed(11)
indexR <- sample(1:nrow(colonData), 20, replace=T)
indexC <- sample(1:ncol(colonData), 10, replace=T) 
colonSample <-colonData[indexR, indexC]
indexR
indexC
colonSample

colonSample[15,3] <- NA
colonSample

library(e1071)
# Continuous variable ---> use mean imputation
fixColonSample1 <- impute(colonSample[ , 1:10], what = 'mean')

# Categorical variable ---> use mode or median imputation
# Convert numeric to factor
colonSample[,1] <- cut(colonSample[,1], breaks = 3, labels = c('1','2','3'))
colonSample[,1] <-as.numeric(colonSample[,1])
colonSample[15, 1] <- NA
colonSample[ 2, 1] <- NA

fixColonSample2 < - impute(colonSample[, 1:10], what = 'median')

# Is there a degree of uncertainty provided by every data imputation techniques?
# What is the impact of uncertainty from every data imputation techniques?

dat <- read.table("../data/imputeExample.csv", header = TRUE, sep=",")
head(dat)

# Sum of all NULL values in every column in the dataframe
sapply(dat, function(x) sum(is.na(x)))


original <- dat
set.seed(10)
dat[sample(1:nrow(dat), 20), "Cholesterol"] <- NA
dat[sample(1:nrow(dat), 20), "Smoking"] <- NA
dat[sample(1:nrow(dat), 20), "Education"] <- NA
dat[sample(1:nrow(dat), 5), "Age"] <- NA
dat[sample(1:nrow(dat), 5), "BMI"] <- NA
# dat[sample(1:nrow(dat), 20), "Cholesterol"] <- 231.23
# dat[sample(1:nrow(dat), 20), "Smoking"] <- 'Yes'
# dat[sample(1:nrow(dat), 20), "Education"] <- 'High'
# dat[sample(1:nrow(dat), 5), "Age"] <- 63.56
# dat[sample(1:nrow(dat), 5), "BMI"] <- 56.87


library(dplyr) 
# Transform the variables into factors or numeric 
dat <-dat %>%
  mutate(Smoking = as.factor(Smoking)) %>% 
  mutate(Education = as.factor(Education)) %>% 
  mutate(Cholesterol = as.numeric(Cholesterol))


library(mice)
init = mice(dat, maxit=0) 
meth = init$method
predM = init$predictorMatrix


meth[c("Cholesterol")]="norm" 
meth[c("Smoking")]="logreg" 
meth[c("Education")]="polyreg"

set.seed(103)
library('Rcpp')

###########################################################################
data2 <- read.table("../data/imputeExample.csv", header = TRUE, sep=",")
head(dat)
set.seed(10)

# Single Imputation with Mode
data2 <- read.table("../data/imputeExample.csv", header = TRUE, sep=",")


set.seed(11)
indexR <- sample(1:nrow(data2), 20, replace=T)
indexC <- sample(1:ncol(data2), 10, replace=T)
data2Sample <- data2[indexR, indexC]
indexR
indexC
data2Sample

data2[sample(1:nrow(dat), 20), "Cholesterol"] <- NA
data2[sample(1:nrow(dat), 20), "Smoking"] <- NA
data2[sample(1:nrow(dat), 20), "Education"] <- NA
data2[sample(1:nrow(dat), 5), "Age"] <- NA
data2[sample(1:nrow(dat), 5), "BMI"] <- NA

data2[,3] <- cut(data2[,3], breaks = 2, labels=c('1', '2'))
data2[,3] <- as.numeric(data2[, 3])

data2[,7] <- cut(data2[,7], breaks = 2, labels=c('1','2'))
data2[,7] <- as.numeric(data2[, 7])

data2[,8] <- cut(data2[,8], breaks = 3, labels=c['1','2','3'])
data2[,8] <- as.numeric(data2[, 8])




imputed <- data2
val <- unique(data2$Smoking[!is.na(data2$Smoking)])
my_mode <- val[which.max(tabulate(match(data2$Smoking, val)))]
imputed$Smoking[is.na(imputed$Smoking)] <- my_mode

sapply(data2, function(x) sum(is.na(x)))
sapply(data2, function(x) typeof(x))
data2


data2[,7] <- cut(data2[,7], breaks = 2, labels=c('1','2'))
data2[,8] <- cut(data2[,8], breaks = 3, labels=c['1','2','3'])

data2 <-data2 %>%
  mutate(Smoking = as.factor(Smoking)) %>% 
  mutate(Education = as.factor(Education)) %>% 
  mutate(Cholesterol = as.numeric(Cholesterol))


# How to transform data
scaleColonSample <- scale(fixColonSample1[, 1:10])
segments <- 5
maxL <- max(colonData$H62245)
minL <- min(colonData$H62245)
theBreaks <- seq(minL, maxL, by=(maxL-minL)/segments)
cutPoints <- cut(colonData$H62245, breaks=theBreaks, include.lowest = T)
newData   <- data.frame(original=colonData$H62245, myCut=cutPoints)
head(newData)


# Merging DataFrame in R
# Perform left-join, right-join, inner-join 
# Create a dataframe

dataset1 <- data.frame(
  brand = c('EROSE', 'Neuroplus'),
  num_models = c(63, 10),
  stringsAsFactors = FALSE
)

dataset2 <- data.frame(
  brand = c('Virgo', 'Contec'),
  num_models = c(26, 4),
  stringsAsFactors = FALSE
)

# RBIND is the same as Python's CONCATENATION
models <- rbind(dataset1, dataset2)
models


reordered_dataset1 <- dataset1[, c(2, 1)]
reordered_dataset1
dataset1

rbind(reordered_dataset1, dataset2)


sales <- data.frame(
  brand = c("Virgo","Neuroplus","Contec","EROSE"), 
  sales = c(19157, 25908, 188328, 29975),
  stringsAsFactors = FALSE)

models
sales
cbind(models, sales)


# LEFT JOIN
results <- merge(x = models, y = sales,by = "brand", all.x=TRUE)
results



salesTemp <-data.frame(
  brand = c("Guangzhou","Hunan"), 
  sales = c(500, 13467),
  stringsAsFactors = FALSE)

sales <- rbind(sales, salesTemp)

resultsRightJoin <- merge(x = models, y = sales,by = "brand", all.y = TRUE)
resultsRightJoin
resultsLeftJoin  <- merge(x = sales, y = models, by = "brand", all.x = TRUE)
resultsLeftJoin

modelsTemp <-data.frame(
  brand = c("Emotiv","NeuroSky"), 
  num_models = c(15,9),
  stringsAsFactors = FALSE)

models <- rbind(models, modelsTemp)
resultsInnerJoin <- merge(x = sales, y = models, by = "brand", all.x = FALSE, all.y = FALSE)
resultsInnerJoin


resultsFullJoin <- merge(x = models, y = sales, by = 'brand', all.x = TRUE, all.y = TRUE)
resultsFullJoin


resultsAssignmentA <- merge(x = models, y = sales, by = 'brand', all.x = FALSE, all.y = FALSE)
resultsAssignmentA
resultsAssignmentB <- merge(x = )


### GROUP BY
modelsTemp <-data.frame(
  brand = c("Emotiv","NeuroSky"), 
  num_models = c(33,16),
  stringsAsFactors = FALSE)
groupbyData <- rbind(models, modelsTemp)
countGroup <- groupbyData %>%
  count(brand)
countGroup

groupbyData %>%
  group_by(brand) %>%
  summarize(mean_num = mean(num_models))

sumGroup <-groupbyData %>%
  group_by(brand) %>%
  summarise(total_num = sum(num_models))

sumGroup

df <-data.frame(
  Weekday = factor(rep(c("Mon", "Tues", "Wed", "Thurs", "Fri"), each = 4),
                   levels = c("Mon", "Tues", "Wed", "Thurs", "Fri")),
  
  Quarter = paste0("Q", rep(1:4, each = 5)),
  
  Delay = c(9.9, 5.4, 8.8, 6.9, 4.9, 9.7, 7.9, 5, 8.8, 11.1, 10.2, 9.3, 12.2,
            10.2, 9.2, 9.7, 12.2, 8.1, 7.9, 5.6))


countGroup <- df %>%
  count(Quarter)
countGroup


groupByMinMax <- df %>%
  group_by(Weekday) %>%
  summarize(min_delay = min(Delay), max_delay = max(Delay))
groupByMinMax

groupBySum <- df %>%
  group_by(Quarter) %>%
  summarize(sum_delay = sum(Delay))
groupBySum


groupByQuarter <-df %>%
  group_by(Quarter) %>%
  summarize(min_delay = min(Delay), max_delay = max(Delay))
groupByQuarter

groupByAndCount1 <-df %>%
  group_by(Weekday, Quarter) %>%
  count()
groupByQuarter


groupByTwoColumn <- df %>%
  group_by(Weekday) %>%
  summarize(min_delay = min(Delay), max_delay = max(Delay), sum_delay = sum(Delay), mean_delay = mean(Delay)) 

groupByTwoColumn


remove1 <- resultsFullJoin[-c(7),]
remove1
remove2 <- resultsFullJoin[-c(2, 4)]
remove2
sapply(remove2, function(x) sum(is.na(x)))

remove4 <- subset(resultsFullJoin, brand == 'Contec')
remove4

remove5 <- subset(resultsFullJoin, brand != "Contec" & sales >= 15000)
remove5


removeCol1 <- subset(resultsFullJoin, select = c(brand, sales))
removeCol1

dataset3 <-data.frame(
  hn = c("121111","333333","444444","121111","444444","555555","888888","666666"), 
  age = c(63,18,25,63,25,33,41,19),
  sex = c("Male","Female","Female","Male","Female","Male","Male","Female"),
  stringsAsFactors = FALSE
)


dataset3
duplicate1 <- distinct(dataset3)
duplicate1


dataset4 <-data.frame(
  glucose = c(90,101,112,89,110,95,99,100),
  stringsAsFactors = FALSE
)
dataset5 <-cbind(dataset3, dataset4)
dataset5
duplicate2 <-distinct(dataset5, hn, age, .keep_all= TRUE)
duplicate2


duplicate4 <-unique(dataset3)
duplicate4

duplicate5 <- unique(dataset5)
duplicate5

duplicate7 <- dataset3[!duplicated(dataset3$hn),]
duplicate7


followUp <- data.frame()