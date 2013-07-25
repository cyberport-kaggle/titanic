train <- read.csv("train.csv", colClasses = c("factor", "factor", "character", "factor", "numeric", "numeric", "numeric", "character", "numeric", "character", "factor"))
test <- read.csv("test.csv", colClasses = c("factor", "character", "factor", "numeric", "numeric", "numeric", "character", "numeric", "character", "factor"))

save(train, test, file = "data.RData")