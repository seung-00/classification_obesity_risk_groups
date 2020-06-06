library(dplyr)
library(foreign)
library(rpart)
library(caret)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(ROCR)
library(readr)

#cleaned_data <- read.csv("D:/Programming/R/classification_obesity_risk_groups/Data/cleaned_data.csv")
cleaned_data <- read.csv("D:/Programming/R/classification_obesity_risk_groups/Data/post_cleaned_data.csv")
cleaned_data <- data.frame(cleaned_data)

unver_attr <- read.csv(file = "D:/Programming/R/classification_obesity_risk_groups/Data/결측치(모름)_조사(unf).csv")
unver_attr <- data.frame(unver_attr)
unver_attr <- filter(unver_attr, unver_attr$type == 1 | unver_attr$type == 3)

for (i in 1:nrow(unver_attr)) {
    #print(unver_attr[i, "value_name"])
    if (as.character(unver_attr[i, "value_name"]) %in% names(cleaned_data)){
        cleaned_data[, as.character(unver_attr[i, "value_name"])] <- as.factor(cleaned_data[, as.character(unver_attr[i, "value_name"])])
    }
}
# wt가 붙은 가중치 속성 제거
cleaned_data <- cleaned_data[, - grep("wt_", names(cleaned_data))]
# HE_가 붙은 신체 수치 속성 제거
cleaned_data <- cleaned_data[, - grep("HE_", names(cleaned_data))]
cleaned_data <- select(cleaned_data, -kstrata)



train_no <- cleaned_data %>% filter(cleaned_data$danger == 0)
train_yes <- cleaned_data %>% filter(cleaned_data$danger == 1)

# Train 셋을 리샘플링(Yes와 No를 1:1 비율로)
train_no_ran_sam <- sample(1:nrow(train_no), nrow(train_yes))
train_no <- train_no[train_no_ran_sam,]
resampled_df <- rbind(train_no, train_yes)

# test, training셋 분리
set.seed(1000)
intrain <- createDataPartition(y = resampled_df$danger, p = 0.8, list = F)
train <- resampled_df[intrain,]
test <- resampled_df[-intrain,]

get_set_from_file <- function() {
    train <- read.csv("Data/tree_train.csv")
    train <- data.frame(train)

    train <- train %>% select(-X)

    for (i in 1:nrow(unver_attr)) {
        if (as.character(unver_attr[i, "value_name"]) %in% names(train)){
            train[, as.character(unver_attr[i, "value_name"])] <- as.factor(train[, as.character(unver_attr[i, "value_name"])])
        }
    }

    return(train)
}

write_set_to_file <- function(tr) {
    write.csv(tr, "Data/tree_train.csv")
}

plot_best_tree_on_wid <- function(t) {
    win.graph()
    ptree <- prune(t, cp = t$cptable[which.min(t$cptable[, "xerror"]), "CP"])

    fancyRpartPlot(ptree)
}

# 저장된 훈련셋을 이용할 때 밑 라인의 주석을 제거하면 됨
#train <- get_set_from_file()

# tree 새로 그릴 때
t <- rpart(danger ~ ., data = train, method = 'class')
# pre_train 셋 재현할 때
#t <- rpart(danger ~ BO2_1 + age + GS_mea_r_1 + N_WAT_C + MH1_yr + GS_mea_r_3 + BD2 + N_N3 +  L_OUT_FQ + apt_t + L_BR_FQ, data = train, method = 'class')
# post_train 셋 재현할 때
#t <- rpart(danger ~ BO2_1 + age + GS_mea_l_2 + educ + GS_mea_r_3 + LK_LB_IT + BE3_32 + ainc +  N_FE + N_WAT_C + BD2 + N_B1 + EC_wht_23, data = train, method = 'class')

for (i in 1:nrow(t$cptable)) {
    print(i, t$cptable[i, "CP"])
    ptree <- prune(t, cp = t$cptable[i, "CP"])

    pred <- predict(ptree, test, type = "class")
    print(confusionMatrix(pred, as.factor(test$danger)))

    pred <- predict(ptree, test, type = "prob")
    pred <- prediction(pred[, 2], test$danger)
    prf <- performance(pred, "tpr", "fpr")
    win.graph()
    plot(prf)
    abline(0, 1, lty = 2)
    print(performance(pred, "auc"))
}

plot_best_tree_on_wid(t)