if(TRUE)"
modeling.r은 pre_selected_data 혹은 post_selected_data.csv를 대상으로 lasso 로지스틱 회귀 모형을 수행합니다.
아래 코드는 pre_selected_data 를 읽는 부분이 주석처리되어 있습니다.
모델 평가 결과는 마지막에 주석으로 남겨놨습니다.
쓰이는 방법론으로는 lsso 회귀 모형이 있으며 사용되는 평가 지표는 혼동 행렬, ROC Curve 이고 lasso 의 안정적인 성능을 위해 10-fold 교차검증이 동반됩니다.
"


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

setwd("/Users/seungyoungoh/workspace/classification_obesity_risk_groups/Data/")

#data <- read.csv("LogisticRegression_Results/pre_selected_data.csv", header=TRUE, stringsAsFactors = TRUE, na.strings=NA)

data <- read.csv("LogisticRegression_Results/post_selected_data.csv", header=TRUE, stringsAsFactors = TRUE, na.strings=NA)


unver_attr <- read.csv(file = "결측치(모름)_조사(unf).csv")
unver_attr <- data.frame(unver_attr)
unver_attr <- filter(unver_attr, unver_attr$type == 1 | unver_attr$type == 3)

my_colnames <- c(colnames(data))

# 전체 결측치(모름) 자료 조사 중 타입이 1,3 으로 factor에 해당하는 경우에 내 데이터의 칼럼명이 있다면 내 데이터의 타입을 바꿔준다.

for(i in 1:nrow(unver_attr))
{
    factor_name_from_tot <- unver_attr[i,1]
    if(factor_name_from_tot %in% my_colnames)
    {
        data[, as.character(unver_attr[i, "value_name"])] <- as.factor(data[, as.character(unver_attr[i, "value_name"])])
    }
}

str(data)

# danger 팩터로
data$danger <- factor(data$danger, levels = c(1,0))
lasso_data<-data

dim(lasso_data)
# [1] 1256   67
# [1] 1256   80

# 파티션
set.seed(2020)
input_train <- createDataPartition(y=lasso_data$danger, p=0.8, list=FALSE)
train_dataset <- lasso_data[input_train,]
test_dataset <- lasso_data[-input_train,]

dim(train_dataset)
# [1] 1006 67
# [1] 1006 80

dim(test_dataset)
# [1] 250  67
# [1] 250  80

### lasso 모델링, 교차검증
# auc 계산
lasso_model <- cv.glmnet( x=data.matrix(train_dataset[,-length(train_dataset)]), y = train_dataset[,length(train_dataset)],
family = "binomial" , type.measure = "auc",alpha=1, nfolds=10)

print(lasso_model)
plot(lasso_model)

coef(lasso_model, s=lasso_model$lambda.1se)
as.matrix(coef(lasso_model, lasso_model$lambda.1se))

# confusion matrix로 성능 계산
lasso_pred <- predict(lasso_model, newx=data.matrix(test_dataset[,-length(test_dataset)]),
                      s=lasso_model$lambda.1se, type= "class", levels=c(1,0))

confusionMatrix(table(factor(test_dataset[,length(test_dataset)], levels=c(1,0)),factor((lasso_pred), levels = c(1,0))))

### pre confusionMatrix result
#      1  0
#   1 73 52
#   0 44 81
                                          
#                Accuracy : 0.616           
#                  95% CI : (0.5526, 0.6766)
#     No Information Rate : 0.532           
#     P-Value [Acc > NIR] : 0.004509        
                                          
#                   Kappa : 0.232           
                                          
#  Mcnemar's Test P-Value : 0.474959        
                                          
#             Sensitivity : 0.6239          
#             Specificity : 0.6090          
#          Pos Pred Value : 0.5840          
#          Neg Pred Value : 0.6480          
#              Prevalence : 0.4680          
#          Detection Rate : 0.2920          
#    Detection Prevalence : 0.5000          
#       Balanced Accuracy : 0.6165          
                                          
#        'Positive' Class : 1                    




## post confusionMatrix result
#      1  0
#   1 81 44
#   0 41 84
                                          
#                Accuracy : 0.66            
#                  95% CI : (0.5976, 0.7185)
#     No Information Rate : 0.512           
#     P-Value [Acc > NIR] : 1.57e-06        
                                          
#                   Kappa : 0.32            
                                          
#  Mcnemar's Test P-Value : 0.8283          
                                          
#             Sensitivity : 0.6639          
#             Specificity : 0.6562          
#          Pos Pred Value : 0.6480          
#          Neg Pred Value : 0.6720          
#              Prevalence : 0.4880          
#          Detection Rate : 0.3240          
#    Detection Prevalence : 0.5000          
#       Balanced Accuracy : 0.6601          
                                          
#        'Positive' Class : 1               

## ROC curve, auc
y_obs <- ifelse(test_dataset$danger==1,0,1)
yhat_glmnet <- predict(lasso_model, s="lambda.1se", newx=data.matrix(test_dataset[,-length(test_dataset)]), type="response", levels=c(1,0))

yhat_glmnet <- yhat_glmnet[,1]
pred_glment <- prediction(yhat_glmnet, y_obs)
pref_glment <- performance(pred_glment, measure="tpr", x.measure = "fpr")
plot(pref_glment, col='red', main="ROC Curve")
abline(0,1)
performance(pred_glment, "auc")@y.values[[1]][1]
# 0.702784
# 0.72224