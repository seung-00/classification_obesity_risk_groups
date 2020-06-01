library(dplyr)
library(foreign)
library(rpart)
library(caret)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(ROCR)
library(readr)

cleaned_data <- read.csv("D:/Programming/R/classification_obesity_risk_groups/Data/cleaned_data(모름 유지).csv")
cleaned_data <- data.frame(cleaned_data) 

unver_attr <- read.csv(file = "D:/Programming/R/classification_obesity_risk_groups/Data/결측치(모름)_조사(unf).csv")
unver_attr <- data.frame(unver_attr)
unver_attr <- filter(unver_attr, unver_attr$타입 == 1 | unver_attr$타입 == 3)

for (i in 1:nrow(unver_attr)) {
    #print(unver_attr[i, "변수명"])
    cleaned_data[, as.character(unver_attr[i, "변수명"])] <- as.factor(cleaned_data[, as.character(unver_attr[i, "변수명"])])
}

# wt가 붙은 가중치 속성 제거
cleaned_data <- cleaned_data[, - grep("wt_", names(cleaned_data))]
# ID와 같이 오브젝트를 유니크하게 식별할 수 있는 범주형 속성을 삭제
cleaned_data <- cleaned_data %>% select(-X, -ID, -ID_fam, -psu, -year, -mod_d, -DC11_tp, -M_2_et, -BH9_14_4_02, -N_DT_DS, -N_DT_DS, -AC3_3e_01, -AC3_3e_02, -LQ4_24, -BH9_14_4_01, -N_DAY, -BM14_2, -BO3_11, -EC_wht_6, -BS5_31, -BS12_35) %>%
# BMI 수치와 같이 분석에 직접 영향을 주는 변수를 삭제
                select(-HE_obe, - BO1_3,- BO1_1, - HE_wt,
                            - HE_BMI, - BO1_2, - HE_wc, - BO1)
                            
# HE_가 붙은 신체 수치 속성 제거
cleaned_data <- cleaned_data[, - grep("HE_", names(cleaned_data))]



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
        if (as.character(unver_attr[i, "변수명"]) %in% names(train)){
            train[, as.character(unver_attr[i, "변수명"])] <- as.factor(train[, as.character(unver_attr[i, "변수명"])])
        }
    }

    return(train)
}

write_set_to_file <- function(tr) {
    write.csv(tr, "tree_train.csv")
}

# tree 그리기
t <- rpart(danger ~ ., data = train, method = 'class', control = rpart.control(minsplit = 2, minbucket = 1, cp = 0.0059))

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