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
  return(temp_C)
}
fahrenheit_to_celsius(90)
