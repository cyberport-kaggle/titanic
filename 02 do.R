library(caret)
library(doMC)
registerDoMC(4)

load("data.RData")
train$cabin <- substr(train$cabin, 1, 1)

relpred <- c("pclass", "sex", "age", "sibsp", "parch", "fare", "embarked")

# Get the training data into correct data types
train = train[, c("survived", relpred)]
train = gbmImpute(train)$x
train$age = as.numeric(train$age)

xtrain  = model.matrix(survived ~ . , data = train)

preproc = preProcess(xtrain, method=c("center", "scale"))
xtrain = predict(preproc, xtrain)

ytrain = train[, "survived"]

# Get the test data into correct data types
test = gbmImpute(test[, relpred])$x
test = model.matrix(~. , test)
test = predict(preproc, test)

fitControl <- trainControl(method="cv", number=5)
svmFit <- train(ytrain, xtrain,
                method = "rf", tuneLength = 10, trControl = fitControl)
svmFit

pred = predict(svmFit, test)
write.table(cbind(pred, test), file = "pred.csv", row.names = FALSE, col.names = FALSE, sep = ",")
# 
# str(train)