autism <- read.csv('../data/Autism-Adolescent-Data.csv')
leukem <- read.csv('../data/leukemiaData.csv')
library(ggplot2)
library(GGally)
sapply(autism, function(x) sum(is.na(x)))
sapply(leukem, function(x) sum(is.na(x)))

sapply(autism, function(x) typeof(x))