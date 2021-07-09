library(MASS)
data(Boston); #this data is in MASS package
colnames(Boston)
dim(Boston)
names(Boston)
str(Boston)
summary(Boston)
library(Hmisc)
par(mar=c(1,1,1,1))
hist.data.frame(Boston)

library(purrr)
library(tidyr)
library(ggplot2)

Boston %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()



require(ggplot2)
ggplot(data = Boston, aes(x=variable, y=value)) + geom_boxplot(aes(fill=Label))
set.seed(13960406)
sample_index <- sample(nrow(Boston),nrow(Boston)*0.80)
Boston_train <- Boston[sample_index,]
Boston_test <- Boston[-sample_index,]

model_1 <- lm(medv~., data=Boston_train)
summary(model_1)

low_model=lm(medv~1, data=Boston_train)
upper_model=lm(medv~., data=Boston_train)


model_aic <- MASS::stepAIC(low_model,direction="forward",scope=list(upper=upper_model,lower=low_model))
summary(model_aic)
AIC(model_aic)
model_bic <- step(low_model,direction="forward",scope=list(upper=upper_model,lower=low_model), k = log(nrow(Boston_train)))
summary(model_bic)
AIC(model_bic)
library(glmnet)
lasso_fit <- glmnet(x = as.matrix(Boston[, -c(which(colnames(Boston)=='medv'))]), y = Boston$medv, alpha = 1)
#lambda = 0.5
coef(lasso_fit,s=0.5)
model_lasso <- lm(medv ~ crim + lstat + rm + ptratio + dis + nox + black + chas, data=Boston_train)
AIC(model_lasso)
#MSE
model_summary <- summary(model_aic)
(model_summary$sigma)^2

#out of sample
pi <- predict(object = model_aic, newdata = Boston_test)
pi <- predict(model_aic, Boston_test)
mean((pi - Boston_test$medv)^2)

final_model <- glm(medv~lstat + rm + ptratio + black + dis + nox + 
                     zn + chas + rad + tax + crim, data = Boston)
library(boot)
cv.glm(data = Boston, glmfit = final_model, K = 5)$delta[2]

#regression tree
library(rpart)
library(rpart.plot)

boston_rpart <- rpart(formula = medv ~ ., data = Boston_train)
boston_rpart
prp(boston_rpart,digits = 4, extra = 1)

boston_train_pred_tree = predict(boston_rpart,Boston_train)
MSE.tree <- mean((boston_train_pred_tree -Boston_train$medv)^2)
MSE.tree

boston_test_pred_tree = predict(boston_rpart,Boston_test)
MSPE.tree <- mean((boston_test_pred_tree -Boston_test$medv)^2)
print(paste("The Regression Tree model’s out-of-sample MSE is " ,MSPE.tree))

#Out-of-sample prediction
boston_test_pred_tree = predict(boston_rpart,Boston_test)
#compare this model’s out-of-sample performance with the linear regression model with all variables in it
# boston.reg = lm(medv~., data = Boston_train)
boston_test_pred_reg = predict(model_aic, Boston_test)
mean((boston_test_pred_reg - Boston_test$medv)^2)
#randomsample 2
set.seed(139604060)
sample_index1 <- sample(nrow(Boston),nrow(Boston)*0.80)
Boston_train1 <- Boston[sample_index1,]
Boston_test1 <- Boston[-sample_index1,]
dim(Boston_train1)
model_2 <- lm(medv~., data=Boston_train1)
summary(model_2)

low_model1=lm(medv~1, data=Boston_train1)
upper_model1=lm(medv~., data=Boston_train1)


model_aic2 <- MASS::stepAIC(low_model1,direction="forward",scope=list(upper=upper_model1,lower=low_model1))
model_bic2 <- step(low_model1,direction="forward",scope=list(upper=upper_model1,lower=low_model1), k = log(nrow(Boston_train1)))
AIC(model_aic2)
AIC(model_bic2)
library(glmnet)
lasso_fit <- glmnet(x = as.matrix(Boston[, -c(which(colnames(Boston)=='medv'))]), y = Boston$medv, alpha = 1)
#lambda = 0.5
coef(lasso_fit,s=0.5)
model_lasso <- lm(medv ~ crim + lstat + rm + ptratio + dis + nox + black + chas, data=Boston_train1)
AIC(model_lasso)
#MSE
model_summary <- summary(model_aic2)
(model_summary$sigma)^2

#out of sample
pi <- predict(object = model_aic2, newdata = Boston_test1)
pi <- predict(model_aic2, Boston_test1)
mean((pi - Boston_test1$medv)^2)

final_model2 <- glm(medv~lstat + rm + ptratio + black + dis + nox + 
                      zn + chas + rad + tax + crim, data = Boston)
library(boot)
model_full1 <- glm(medv~., data = Boston)
cv.glm(data = Boston, glmfit = final_model2, K = 5)$delta[2]

#regression tree
library(rpart)
library(rpart.plot)

boston_rpart <- rpart(formula = medv ~ ., data = Boston_train1)
boston_rpart
prp(boston_rpart,digits = 4, extra = 1)

boston_test_pred_tree = predict(boston_rpart,Boston_test1)
MSPE.tree <- mean((boston_test_pred_tree -Boston_test1$medv)^2)
print(paste("The Regression Tree model’s out-of-sample MSE is " ,MSPE.tree))



## TREE-BASED MODELS{.tabset .tabset-fade .tabset-pills}

### REGRESSION TREE

We will now fit a regression tree to the dataset.
```{r,message=FALSE,warning=FALSE}
#Regression tree
library(rpart)
library(rpart.plot)
boston_rpart <- rpart(formula = medv ~ ., data = boston_train)
boston_rpart
prp(boston_rpart,digits = 4, extra = 1)
```

We can notice that the results indicates that only four of the variables have been used to construct the tree.

We can now check the in-sample performance of the tree by calculating the tree MSE.
```{r,message=FALSE,warning=FALSE}
#In-sample prediction
boston_train_pred_tree = predict(boston_rpart)
MSE_train_tree <- mean((boston_train_pred_tree - boston_train$medv)^2)
print(paste("The mean squared error (MSE) of the in-sample for this tree model is " ,MSE_train_tree))
```

We will now use the model to calculate out-of-sample performance of the tree.
```{r,message=FALSE,warning=FALSE}
#Out-of-sample prediction
boston_test_pred_tree = predict(boston_rpart,boston_test)
MSPE.tree <- mean((boston_test_pred_tree -boston_test$medv)^2)
print(paste("The Regression Tree model’s out-of-sample MSE is " ,MSPE.tree))
```

### BAGGING

We will now apply bagging to the dataset.
```{r,message=FALSE,warning=FALSE}
#Bagging
library(ipred)
boston_bag<- bagging(formula = medv~., 
                     data = boston_train, 
                     nbagg=100)
boston_bag
```

We will check the in sample performance of the model.
```{r,message=FALSE,warning=FALSE}
#in-sample
boston_bag_pred<- predict(boston_bag, newdata = boston_train)
mean((boston_train$medv-boston_bag_pred)^2)
```

The in sample MSE is calculated to be 10.79223.

We will now check the out-of-sample performance of the model using in-sample MSE.
```{r,message=FALSE,warning=FALSE}
#in-sample
boston_bag_pred<- predict(boston_bag, newdata = boston_test)
mean((boston_test$medv-boston_bag_pred)^2)
```

We can find that the test MSE of the model is calculated to be 18.58307 which is less than the MSE obserbved from regression tree model.

We can evaluate the out of bag performance of this model by using the testing dataset.
```{r,message=FALSE,warning=FALSE}
#out of bag
boston_bag_oob<- bagging(formula = medv~., 
                         data = boston_train, 
                         coob=T, 
                         nbagg=100)
boston_bag_oob
```

### RANDOM FOREST

Now, we will grow a Random forest on the dataset. It works exactly the same way as bagging except that it uses a smaller value of the 'mtry' argument. We are using p/3 variables which is the default for building a randomForest() of regression trees.
```{r,message=FALSE,warning=FALSE}
#random forest
library(randomForest)
boston_rf<- randomForest(medv~., data = boston_train, importance=TRUE)
boston_rf
```

The in sample MSE of the random forest model is 10.94049.

Now, let us check the prediction on testing sample.
```{r,message=FALSE,warning=FALSE}
#out-of-sample
boston_rf_pred<- predict(boston_rf, boston_test)
mean((boston_test$medv-boston_rf_pred)^2)
```

The test MSE of the model is 10.84043. This indicates that random forests yielded an improvement over bagging in this case.

### BOOSTING

Now, we will use the gbm() function to fit boosted regression trees to the dataset.
```{r,message=FALSE,warning=FALSE}
#Boosting
library(gbm)
boston_boost<- gbm(formula = medv~., 
                   data = boston_train, 
                   distribution = "gaussian", 
                   n.trees = 10000, 
                   shrinkage = 0.01, 
                   interaction.depth = 8)
summary(boston_boost)
```

We can observe a relative influence plot and the relative influence statistics.We can see that lstat and rm are the most important variables.We can also produce partial dependence plots of these two variables. 
```{r,message=FALSE,warning=FALSE}
par(mfrow=c(1,2))
plot(boston_boost, i="lstat")
plot(boston_boost, i="rm")
```

We can see that median house prices are increasing with rm and decreasing with lstat as expected.

We can check the in sample performance of the model.
```{r,message=FALSE,warning=FALSE}
boston_boost_pred_train<- predict(boston_boost, boston_train, n.trees = 10000)
mean((boston_train$medv-boston_boost_pred_train)^2)
```

The in sample MSE is calculated to be 

We can now use the boosted model to predict medv on the test set:
  ```{r,message=FALSE,warning=FALSE}
#out-of-sample
boston_boost_pred_test<- predict(boston_boost, boston_test, n.trees = 10000)
mean((boston_test$medv-boston_boost_pred_test)^2)
```

The test MSE obtained is 10.89395 almost similar to the test MSE for random forests and better than that for bagging. 

