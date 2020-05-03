# install.packages("cran")
library(sampling)
library(car)
library(dplyr)
library(foreign)
library(caret)
library(ggplot2)
library(glmnet)
library(ROCR)


setwd("/Users/seungyoungoh/workspace/classification_obesity_risk_groups/Data")
# cleaned_data <- read.csv("cleaned_data.csv", header=TRUE, stringsAsFactors = TRUE, na.strings=NA)
selected_data.csv <- read.csv("selected_data.csv", header=TRUE, stringsAsFactors = TRUE, na.strings=NA)

# 팩터의 순서를 1,0으로 만들어 줘야함. 그래야 positive = 1, negative = 0 으로 glm이 인식함

# danger 팩터로
selected_data$danger <- factor(selected_data$danger, levels = c(1,0))
lasso_data<-selected_data

dim(lasso_data)
# [1] 960  58

# 파티션
input_train <- createDataPartition(y=lasso_data$danger, p=0.8, list=FALSE)
train_dataset <- lasso_data[input_train,]
test_dataset <- lasso_data[-input_train,]

# auc 계산
lasso_model <- cv.glmnet( x=data.matrix(train_dataset[,-length(train_dataset)]), y = train_dataset[,length(train_dataset)],
family = "binomial" , type.measure = "auc",alpha=1, nfolds=5)

print(lasso_model)
plot(lasso_model)

lasso_model <- cv.glmnet( x=data.matrix(train_dataset[,-length(train_dataset)]), y = train_dataset[,length(train_dataset)],
family = "binomial" , type.measure = "class",alpha=1, nfolds=5)

print(lasso_model)
plot(lasso_model)

# confusion matrix로 성능 계산
lasso_pred <- predict(lasso_model, newx=data.matrix(test_dataset[,-length(test_dataset)]),
                      s=lasso_model$lambda.min, type= "class", levels=c(1,0))

table(real = test_dataset[,length(test_dataset)],pred= factor((lasso_pred), levels = c(1,0)))

#     pred
# real  1  0
#    1 63 33
#    0 36 60
   
confusionMatrix(table(factor(test_dataset[,length(test_dataset)], levels=c(1,0)),factor((lasso_pred), levels = c(1,0))))

#                Accuracy : 0.6406          
#                  95% CI : (0.5684, 0.7084)
#     No Information Rate : 0.5156 
#     P-Value [Acc > NIR] : 0.0003158       
                                          
#                   Kappa : 0.2812          
                                          
#  Mcnemar's Test P-Value : 0.8097321       
                                          
#             Sensitivity : 0.6364          
#             Specificity : 0.6452          
#          Pos Pred Value : 0.6562          
#          Neg Pred Value : 0.6250          
#              Prevalence : 0.5156          
#          Detection Rate : 0.3281          
#    Detection Prevalence : 0.5000          
#       Balanced Accuracy : 0.6408          
                                          
#        'Positive' Class : 1