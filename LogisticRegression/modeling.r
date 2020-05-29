#  install.packages("gbm")
library(gridExtra)
library(foreign)
library(ggplot2)
library(glmnet)
library(dplyr)
library(caret)
library(ROCR)
library(ISLR)
library(car)

setwd("/Users/seungyoungoh/workspace/classification_obesity_risk_groups/Data")
selected_data <- read.csv("selected_data.csv", header=TRUE, stringsAsFactors = TRUE, na.strings=NA)

# 팩터의 순서를 1,0으로 만들어 줘야함. 그래야 positive = 1, negative = 0 으로 glm이 인식함

# danger 팩터로
selected_data$danger <- factor(selected_data$danger, levels = c(1,0))
lasso_data<-selected_data

dim(lasso_data)
# [1] 960  62

# 파티션
set.seed(2005)
input_train <- createDataPartition(y=lasso_data$danger, p=0.8, list=FALSE)
train_dataset <- lasso_data[input_train,]
test_dataset <- lasso_data[-input_train,]

dim(train_dataset)
# [1] 768  62
dim(test_dataset)
# [1] 192  62

### lasso 모델링, 교차검증
# auc 계산
lasso_model <- cv.glmnet( x=data.matrix(train_dataset[,-length(train_dataset)]), y = train_dataset[,length(train_dataset)],
family = "binomial" , type.measure = "auc",alpha=1, nfolds=5)

print(lasso_model)
#       Lambda Measure
# min 0.000974  0.7609
# 1se 0.006870  0.7505
plot(lasso_model)

coef(lasso_model, s=lasso_model$lambda.1se)

as.matrix(coef(lasso_model, lasso_model$lambda.1se))

# confusion matrix로 성능 계산
lasso_pred <- predict(lasso_model, newx=data.matrix(test_dataset[,-length(test_dataset)]),
                      s=lasso_model$lambda.1se, type= "class", levels=c(1,0))

confusionMatrix(table(factor(test_dataset[,length(test_dataset)], levels=c(1,0)),factor((lasso_pred), levels = c(1,0))))
     
#      1  0
#   1 66 30
#   0 33 63                                          
#                Accuracy : 0.6719          
#                  95% CI : (0.6006, 0.7378)
#     No Information Rate : 0.5156          
#     P-Value [Acc > NIR] : 8.289e-06       
                                          
#                   Kappa : 0.3438          
                                          
#  Mcnemar's Test P-Value : 0.8011          
                                          
#             Sensitivity : 0.6667          
#             Specificity : 0.6774          
#          Pos Pred Value : 0.6875          
#          Neg Pred Value : 0.6562          
#              Prevalence : 0.5156          
#          Detection Rate : 0.3438          
#    Detection Prevalence : 0.5000          
#       Balanced Accuracy : 0.6720          
                                          
#        'Positive' Class : 1              

# ROC curve, auc
y_obs <- ifelse(test_dataset$danger==1,0,1)
yhat_glmnet <- predict(lasso_model, s="lambda.1se", newx=data.matrix(test_dataset[,-length(test_dataset)]), type="response", levels=c(1,0))

yhat_glmnet <- yhat_glmnet[,1]
pred_glment <- prediction(yhat_glmnet, y_obs)
pref_glment <- performance(pred_glment, measure="tpr", x.measure = "fpr")
plot(pref_glment, col='black', main="ROC Curve")
abline(0,1)
performance(pred_glment, "auc")@y.values[[1]]
# [1] 0.7307943
