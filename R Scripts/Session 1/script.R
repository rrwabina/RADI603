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


