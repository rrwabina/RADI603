x <- 5
y <- c(5, 9, 3)
meany = mean(y)
print(meany)

a <- c('Kolkata','Rome','New York', 'London', 'Melbourne')
b <- c(2,3,4,5)
c <- matrix(list(1,"a", 1+2i, TRUE), 2, 2)
d <- list(2, 'London', 'YES')
print(a)
print(b)
print(c)


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

list_data <- list("Red", "Green", c(21,32,11), TRUE, 51.23, 119.1)