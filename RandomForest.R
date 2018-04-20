
train = read.csv("train.csv", header = TRUE)
test  = read.csv("test.csv",  header = TRUE)

test$test_id <- seq.int(nrow(test))
summary(test)

library('randomForest')

###logistic regression
###

set.seed(71)
randomForest_model  = randomForest(SeriousDlqin2yrs ~ ., data=train, importance=TRUE,proximity=TRUE)

test1 = predict(randomForest_model, newdata = test)

###output

my_rf_prediction = data.frame(Id = test$test_id, 	Probability = test1)

### Save to local drive for submission:
###

write.csv(my_rf_prediction, "my_RandomForest_prediction.csv", row.names = F)
