library(sampling)
library(car)
library(dplyr)
library(foreign)
library(caret)
library(ggplot2)
library(glmnet)


setwd("/Users/seungyoungoh/workspace/classification_obesity_risk_groups/Data")
cleaned_data <- read.csv("cleaned_data.csv", header=TRUE, stringsAsFactors = TRUE, na.strings=NA)


# 전진 선택
Model_full <- glm(danger~.,family=binomial(link = 'logit'),data=cleaned_data)
Model_non <- glm(danger~1,family=binomial(link = 'logit'),data=cleaned_data)


Model_forward <- step(Model_non,list(lower=Model_non,upper=Model_full),direction = "forward")
summary(Model_forward)

# 전진 선택 결과
# glm(formula = danger ~ age , HE_Uacid , BO2_1 , BP_PHQ_5 , HE_dbp3 , 
#     HE_DM , DI6_ag , BD7_64 , BA2_12 , BH1 , BP16_22 , LQ4_07 , 
#     DJ2_dg , HE_Bplt , BA2_2_1 , BD7_5 , BS12_31 , O_chew_d , 
#     HE_mens , GS_mea_l_1 , LQ4_05 , HE_Upro , L_DN_WHO , ainc_1 , 
#     DH4_dg , L_BR_TO , HE_Ucot , DC3_dg , LK_EDU , BE8_2 , BS3_3 , 
#     occp , HE_glu , LQ_4EQL , BS5_1 , EC_pedu_2 , T_Q_HR1 , DF2_pr , 
#     DE1_pt , DE1_ag , HE_PLS , BM2_1 , N_VITC , DH3_ag , DC4_dg , 
#     AC1_yr , MO4_11 , MO4_12 , HE_sbp1 , edu , BM8 , house , 
#     DH3_pt , BS12_2 , DH4_ag , HE_HPfh1 , HE_Uph , N_MUFA , DF2_ag , BP_PHQ_8 , N_PROT , N_NIAC , live_t , LQ4_13)




# 팩터의 순서를 1,0으로 만들어 줘야함. 그래야 positive = 1, negative = 0 으로 glm이 인식함
# danger 팩터로
cleaned_data$danger <- factor(cleaned_data$danger, levels = c(1,0))

# 선택된 피쳐만 고르고
lasso_data <- cleaned_data <- cleaned_data%>% select(age , HE_Uacid , BO2_1 , BP_PHQ_5 , HE_dbp3 , 
    HE_DM , DI6_ag , BD7_64 , BA2_12 , BH1 , BP16_22 , LQ4_07 , 
    DJ2_dg , HE_Bplt , BA2_2_1 , BD7_5 , BS12_31 , O_chew_d , 
    HE_mens , GS_mea_l_1 , LQ4_05 , HE_Upro , L_DN_WHO , ainc_1 , 
    DH4_dg , L_BR_TO , HE_Ucot , DC3_dg , LK_EDU , BE8_2 , BS3_3 , 
    occp , HE_glu , LQ_4EQL , BS5_1 , EC_pedu_2 , T_Q_HR1 , DF2_pr , 
    DE1_pt , DE1_ag , HE_PLS , BM2_1 , N_VITC , DH3_ag , DC4_dg , 
    AC1_yr , MO4_11 , MO4_12 , HE_sbp1 , edu , BM8 , house , 
    DH3_pt , BS12_2 , DH4_ag , HE_HPfh1 , HE_Uph , N_MUFA , DF2_ag , BP_PHQ_8 , N_PROT , N_NIAC , live_t , LQ4_13, danger)

dim(lasso_data)
# [1] 2699   65
# 로지스틱 회귀 돌려본다
glm_fit <- glm(danger~.,data = lasso_data, family = binomial(link = 'logit'))

# 다중 공선성 판단
# VIF 수식의 값이 10 이상 이면 해당 변수가 다중공선성이 존재하는 것으로 판단한다.
vif(glm_fit)
#         age    HE_Uacid       BO2_1    BP_PHQ_5     HE_dbp3       HE_DM 
#    2.576141    1.673417    1.104689    1.189999    2.221682    2.829153 
#      DI6_ag      BD7_64      BA2_12         BH1     BP16_22      LQ4_07 
#    1.014915    1.050697    1.383315    1.162532    1.036226 2788.506182 
#      DJ2_dg     HE_Bplt     BA2_2_1       BD7_5     BS12_31    O_chew_d 
#    1.015536    1.108853    1.102630    1.000004    1.060352    1.332740 
#     HE_mens  GS_mea_l_1      LQ4_05     HE_Upro    L_DN_WHO      ainc_1 
#    3.873570    3.157899 2344.610468    1.133784    1.127017    1.272584 
#      DH4_dg     L_BR_TO     HE_Ucot      DC3_dg      LK_EDU       BE8_2 
#   17.810133    1.270718    2.582010    1.000001    1.025721    1.055557 
#       BS3_3        occp      HE_glu     LQ_4EQL       BS5_1   EC_pedu_2 
#    1.327366    1.315127    2.627219    1.277512    2.962979    1.646517 
#     T_Q_HR1      DF2_pr      DE1_pt      DE1_ag      HE_PLS       BM2_1 
#    1.159261  208.821258 1339.780218 1340.415332    1.145818    1.164184 
#      N_VITC      DH3_ag      DC4_dg      AC1_yr      MO4_11      MO4_12 
#    1.057237 2643.169918    1.016957    1.033455  164.148436  163.756583 
#     HE_sbp1         edu         BM8       house      DH3_pt      BS12_2 
#    2.479372    1.987378    1.352498    1.206375 2642.139444    1.405375 
#      DH4_ag    HE_HPfh1      HE_Uph      N_MUFA      DF2_ag    BP_PHQ_8 
#   17.752853    1.068517    1.058625    1.994143  209.723194    1.240847 
#      N_PROT      N_NIAC      live_t      LQ4_13 
#    3.982035    3.490556    1.063495 2534.035070 


# 종속변수의 1, 0 개수를 샌다
table(lasso_data$danger)
#  487 2212

# 불균형 데이터 문제를 해결한다.
# 샘플링
stratified_sampling <- strata(lasso_data, stratanames = c("danger"), size =c(480,480),
                              method="srswor")

new_lasso_data <- getdata(lasso_data, stratified_sampling)
dim(new_lasso_data)
str(new_lasso_data)

# 필요없는 샘플링 결과 값 빼고
new_lasso_data<-new_lasso_data%>%select(-ID_unit,-Prob,-Stratum)
dim(new_lasso_data)
str(new_lasso_data)


# 파티션
input_train <- createDataPartition(y=new_lasso_data$danger, p=0.8, list=FALSE)
train_dataset <- new_lasso_data[input_train,]
test_dataset <- new_lasso_data[-input_train,]

dim(train_dataset)

lasso_model <- cv.glmnet( x=data.matrix(train_dataset[,-length(train_dataset)]), y = train_dataset[,length(train_dataset)],
family = "binomial" , type.measure = "auc",alpha=1, nfolds=5)

lasso_model <- cv.glmnet( x=data.matrix(train_dataset[,-length(train_dataset)]), y = train_dataset[,length(train_dataset)],
family = "binomial" , type.measure = "class",alpha=1, nfolds=5)

str(train_dataset)
plot(lasso_model)

lasso_pred <- predict(lasso_model, newx=data.matrix(test_dataset[,-length(test_dataset)]),
                      s=-4.8, type= "class", levels=c(1,0))

# confusion matrix로 성능 계산
table(actual = test_dataset[,length(test_dataset)],predicted= factor((lasso_pred), levels = c(1,0)))
confusionMatrix(table(factor(test_dataset[,length(test_dataset)], levels=c(1,0)),factor((lasso_pred), levels = c(1,0))))


