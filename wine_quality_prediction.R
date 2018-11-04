setwd("C:/jiaocai/Other/New folder")

# data cleaning
# removing all the outliers;
# turn categorical variables into numerical ones 0,1 to indicate whether it's 0 or not
wine_white = read.csv("white-wine.csv", header=TRUE)
wine_red = read.csv('red-wine.csv',header=TRUE)

# combine two sets
wine=rbind(wine_white,wine_red)
wine=wine[,c(-13)]
wine$quality=as.factor(wine$quality)

# split them into test and train set
# get around 1/5 to be the test set
## 80% of the sample size
smp_size <- floor(0.8 * nrow(wine))

## set the seed to make your partition reproducible
set.seed(775566)
train_ind <- sample(seq_len(nrow(wine)), size = smp_size)

train <- wine[train_ind, ]
test <- wine[-train_ind, ]

# run lasso to do variable selection
# Lasso does variable selection by introducing a l2-norm into the objective
# all the variables no siginicant will be approximated to zero

installed.packages("glmnet")

library(glmnet)

# data cleaning type data frame into matrix

x_mat=as.matrix(train[,c(-13)])
y_mat=as.matrix(train$quality)


# do a cross validation to select the best lamda parameter 

lam_seq=seq(0,1,0.001)

lasso_model = cv.glmnet(x = x_mat, y = y_mat, lambda=lam_seq,alpha = 1)
lasso_coef  = coef(lasso_model, s = lasso_model$lambda.min)

# see all the parameters from lasso 
13 x 1 sparse Matrix of class "dgCMatrix"
                                 1
(Intercept)           94.293850927
fixed.acidity          0.073331049
volatile.acidity      -1.505279930
citric.acid           -0.054103828
residual.sugar         0.059318489
chlorides             -0.699273529
free.sulfur.dioxide    0.006150902
total.sulfur.dioxide  -0.001675778
density              -93.852313301
pH                     0.505642713
sulphates              0.648312654
alcohol                0.242762772
redwine                0.360471788

# we can conclude that variable 6 and 7 are not significant in the model, thus we remove them.
train=train[,-c(6,7)]
test=test[,-c(6,7)]

# run regression models

# first decision tree model
library(rpart)

tree_wine=rpart(quality~., data=train, method='class') 

summary(tree_wine)

# testing the model
y_test1<-predict(tree_wine,newdata=test[,-c(11)],type='class')

# plot tree 
plot(tree_wine, uniform=TRUE, 
  	main="Classification Tree for Wine Quality")
text(tree_wine, use.n=TRUE, all=TRUE, cex=.8)

post(tree_wine, file = "tree.ps", 
  	title = "Classification Tree for Wine Quality")


# calculate the F1-score as a criteria for model

library(caret)

confusionMatrix(y_test1,test$quality,mode = "prec_recall")


Overall Statistics
                                          
               Accuracy : 0.5462          
                 95% CI : (0.5186, 0.5735)
    No Information Rate : 0.4831          
    P-Value [Acc > NIR] : 3.049e-06       
                                          
                  Kappa : 0.1971          
 Mcnemar's Test P-Value : NA              

Statistics by Class:

                     Class: 3 Class: 4 Class: 5 Class: 6
Precision                  NA       NA   0.5633   0.5389
Recall               0.000000  0.00000   0.5228   0.7834
F1                         NA       NA   0.5423   0.6385
Prevalence           0.003846  0.02846   0.3208   0.4831
Detection Rate       0.000000  0.00000   0.1677   0.3785
Detection Prevalence 0.000000  0.00000   0.2977   0.7023
Balanced Accuracy    0.500000  0.50000   0.6657   0.5785
                     Class: 7 Class: 8  Class: 9
Precision                  NA       NA        NA
Recall                 0.0000  0.00000 0.0000000
F1                         NA       NA        NA
Prevalence             0.1362  0.02692 0.0007692
Detection Rate         0.0000  0.00000 0.0000000
Detection Prevalence   0.0000  0.00000 0.0000000
Balanced Accuracy      0.5000  0.50000 0.5000000

'

library('randomForest')

set.seed(71)
randomForest_wine  = randomForest(quality ~ ., data=train, importance=TRUE,proximity=TRUE)

test2 = predict(randomForest_wine, newdata = test[,-c(11)])

confusionMatrix(test2,test$quality,mode = "prec_recall")

Overall Statistics
                                          
               Accuracy : 0.6823          
                 95% CI : (0.6562, 0.7076)
    No Information Rate : 0.4831          
    P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  Kappa : 0.4882          
 Mcnemar's Test P-Value : NA              

Statistics by Class:

                     Class: 3 Class: 4 Class: 5 Class: 6
Precision                  NA 1.000000   0.6841   0.6869
Recall               0.000000 0.135135   0.6906   0.7580
F1                         NA 0.238095   0.6874   0.7207
Prevalence           0.003846 0.028462   0.3208   0.4831
Detection Rate       0.000000 0.003846   0.2215   0.3662
Detection Prevalence 0.000000 0.003846   0.3238   0.5331
Balanced Accuracy    0.500000 0.567568   0.7700   0.7175
                     Class: 7 Class: 8  Class: 9
Precision             0.63473 0.857143        NA
Recall                0.59887 0.342857 0.0000000
F1                    0.61628 0.489796        NA
Prevalence            0.13615 0.026923 0.0007692
Detection Rate        0.08154 0.009231 0.0000000
Detection Prevalence  0.12846 0.010769 0.0000000
Balanced Accuracy     0.77228 0.670638 0.5000000


'

Call:
 randomForest(formula = quality ~ ., data = train, importance = TRUE,      proximity = TRUE) 
               Type of random forest: classification
                     Number of trees: 500
No. of variables tried at each split: 3

        OOB estimate of  error rate: 31.92%
Confusion matrix:
  3  4    5    6   7  8 9 class.error
3 0  1   13   10   1  0 0   1.0000000
4 1 22   85   68   3  0 0   0.8770950
5 0  3 1257  446  15  0 0   0.2696107
6 0  2  367 1690 148  1 0   0.2346014
7 0  0   28  362 508  4 0   0.4368071
8 0  0    2   44  51 61 0   0.6139241
9 0  0    1    0   3  0 0   1.0000000



# by referencing some materials we are able to group wine into poor quality (3,4,5)
# and good quality (6,7,8,9)

wine$quality=as.numeric(wine$quality)

seqloop=seq(1,nrow(wine))

wine$goodquality=NULL

for (val in seqloop)
{
if(wine$quality[val]==3){wine$goodquality[val]=0}
if(wine$quality[val]==4){wine$goodquality[val]=0}
if(wine$quality[val]==5){wine$goodquality[val]=0}
if(wine$quality[val]==6){wine$goodquality[val]=1}
if(wine$quality[val]==7){wine$goodquality[val]=1}
if(wine$quality[val]==8){wine$goodquality[val]=1}
if(wine$quality[val]==9){wine$goodquality[val]=1}
}

# wine$goodquality=as.factor(wine$goodquality)
#wine$goodquality=as.numeric(wine$goodquality)

# binary test

bin_train <- wine[train_ind, ]
bin_test <- wine[-train_ind, ]

# binary lasso

bin_train=bin_train[,-c(13)]
bin_test=bin_test[,-c(13)]

bin_x_mat=as.matrix(bin_train[,-c(13)])
bin_y_mat=as.matrix(bin_train$goodquality)

# do a cross validation to select the best lamda parameter 

lam_seq=seq(0,1,0.001)

lasso_model = cv.glmnet(x = bin_x_mat, y = bin_y_mat, lambda=lam_seq,alpha = 1)
lasso_coef  = coef(lasso_model, s = lasso_model$lambda.min)

13 x 1 sparse Matrix of class "dgCMatrix"
                                 1
(Intercept)           20.437041146
fixed.acidity          0.024493233
volatile.acidity      -0.875899845
citric.acid           -0.092880265
residual.sugar         0.021171621
chlorides             -0.176944880
free.sulfur.dioxide    0.003378141
total.sulfur.dioxide  -0.001353369
density              -21.136866553
pH                     0.198909006
sulphates              0.293156105
alcohol                0.143104834
redwine                0.081307827

# lasso same, remove 6,7

bin_train=bin_train[,-c(6,7)]
bin_test=bin_test[,-c(6,7)]

# run decision tree

bin_tree_wine=rpart(goodquality~., data=bin_train, method='class') 

summary(bin_tree_wine)

# testing the model
bin_test1<-predict(bin_tree_wine,newdata=bin_test[,-c(11)],type='class')

# plot tree 
plot(bin_tree_wine, uniform=TRUE, 
  	main="Classification Tree for Wine Quality")
text(bin_tree_wine, use.n=TRUE, all=TRUE, cex=.8)

post(bin_tree_wine, file = "tree.ps", 
  	title = "Classification Tree for Wine Quality")


confusionMatrix(bin_test1,as.factor(bin_test$goodquality))

# Performance for binary decision tree
Confusion Matrix and Statistics

          Reference
Prediction    0    1
         0 1264   33
         1    0    3
                                          
               Accuracy : 0.9746          
                 95% CI : (0.9645, 0.9825)
    No Information Rate : 0.9723          
    P-Value [Acc > NIR] : 0.3444          
                                          
                  Kappa : 0.1502          
 Mcnemar's Test P-Value : 2.54e-08        
                                          
            Sensitivity : 1.00000         
            Specificity : 0.08333         
         Pos Pred Value : 0.97456         
         Neg Pred Value : 1.00000         
             Prevalence : 0.97231         
         Detection Rate : 0.97231         
   Detection Prevalence : 0.99769         
      Balanced Accuracy : 0.54167         
                                          
       'Positive' Class : 0    '


# run random forest

set.seed(71)
bin_train$goodquality=as.factor(bin_train$goodquality)
bin_test$goodquality=as.factor(bin_test$goodquality)

randomForest_wine_bin  = randomForest(goodquality ~ ., data=bin_train, importance=TRUE,proximity=TRUE)

test_bin2 = predict(randomForest_wine_bin, newdata = bin_test[,-c(11)],type='response')

fac1=as.factor(test_bin2)
fac2=as.factor(bin_test$goodquality)

confusionMatrix(as.factor(test_bin2),as.factor(bin_test$goodquality),mode = "prec_recall")


Confusion Matrix and Statistics

          Reference
Prediction    0    1
         0 1264   24
         1    0   12
                                          
               Accuracy : 0.9815          
                 95% CI : (0.9727, 0.9881)
    No Information Rate : 0.9723          
    P-Value [Acc > NIR] : 0.02106         
                                          
                  Kappa : 0.493           
 Mcnemar's Test P-Value : 2.668e-06       
                                          
              Precision : 0.9814          
                 Recall : 1.0000          
                     F1 : 0.9906          
             Prevalence : 0.9723          
         Detection Rate : 0.9723          
   Detection Prevalence : 0.9908          
      Balanced Accuracy : 0.6667          
                                          
       'Positive' Class : 0               

