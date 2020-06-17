if(TRUE)"
feature_engineering_pre은 '모름'이 대체된 post_cleaned_data.csv를 전처리 후 post_selected_data.csv로 저장합니다
쓰이는 방법론으로는 전진선택, VIF, random undersampling 등이 있습니다.
"

library(sampling)
library(car)
library(dplyr)
library(foreign)
library(caret)
library(ggplot2)
library(glmnet)
library(prettyR)
library(readr)

setwd("/Users/seungyoungoh/workspace/classification_obesity_risk_groups/Data")
cleaned_data <- read.csv("post_cleaned_data.csv")

unver_attr <- read.csv(file = "결측치(모름)_조사(unf).csv")
unver_attr <- data.frame(unver_attr)
unver_attr <- filter(unver_attr, unver_attr$type == 1 | unver_attr$type == 3)

my_colnames <- c(colnames(cleaned_data))
# 전체 결측치(모름) 자료 조사 중 타입이 1,3 으로 factor에 해당하는 경우에 내 데이터의 칼럼명이 있다면 내 데이터의 타입을 바꿔준다.

for (i in 1:nrow(unver_attr)) {
    factor_name_from_tot <- unver_attr[i,1]
    if(factor_name_from_tot %in% my_colnames)
    {
        cleaned_data[, as.character(unver_attr[i, "value_name"])] <- as.factor(cleaned_data[, as.character(unver_attr[i, "value_name"])])
    }
}

                  
# HE_가 붙은 신체 수치 속성 제거
cleaned_data <- cleaned_data[, - grep("HE_", names(cleaned_data))]
# wt가 붙은 가중치 속성 제거
cleaned_data <- cleaned_data[, - grep("wt_", names(cleaned_data))]

dim(cleaned_data)
# [1] 7626  477

# danger 팩터로
cleaned_data$danger <- factor(cleaned_data$danger, levels=c(1,0))
str(cleaned_data)

# 모든 값이 같은 변수 제거
ifelse(n <- sapply(cleaned_data, function(x) length(levels(x))) == 1, "DROP", "")

cleaned_data<- cleaned_data%>%select(-DN6_yd, -DF1_yd, -DJ9_yd, -DI9_yd, -LQ4_17, -LQ4_18, -LQ4_19, -LQ4_20, -CH2_1, -CH2_2)

####  feature selection

backup_data <- cleaned_data


# 전진 선택
Model_full <- glm(danger~.,family=binomial(link = 'logit'),data=cleaned_data)
Model_non <- glm(danger~1,family=binomial(link = 'logit'),data=cleaned_data)


Model_forward <- step(Model_non,list(lower=Model_non,upper=Model_full),direction = "forward")
summary(Model_forward)


# 전진선택결과 2
# glm(formula = danger ~ BO2_1 + GS_mea_l_1 + mh_stress + T_NQ_FIR_P + 
#     LQ_1EQL + L_BR + BM1_4 + BE5_1 + L_LN + DH6_dg + DM1_dg + 
#     age + educ + DE2_dg + BD1_11 + N_PRG + BM2_3 + DC3_pr + DE2_pt + 
#     LQ4_28 + LQ4_03 + DI1_2 + marri_1 + BD2_32 + BE8_1 + DJ8_pr + 
#     BA2_2_4 + LK_LB_CO + BH1 + T_NQ_PH + MH1_yr + BP16_13 + BS12_2 + 
#     BO3_04 + BO3_10 + BO3_09 + BS5_34 + BS5_33 + LQ_5EQL + DC6_pr + 
#     DC3_dg + BS12_1 + M_2_yr + BE3_78 + DC6_pt + BP1 + DI6_ag + 
#     DI6_dg + DI2_2 + GS_mea_l_3 + D_1_1 + DC1_dg + DC1_ag + BM13_4 + 
#     BP16_12 + BP16_22 + EC_wht_23 + N_WAT_C + BM2_5 + DI6_pr + 
#     DM4_dg + N_CHOL + N_FAT + DI1_pr + BO3_12 + DC3_ag + apt_t + 
#     L_DN_FQ + DN1_ag + BS12_43 + BE3_92 + LQ4_15 + LQ4_23 + BS6_4_1 + 
#     BS6_4 + BS6_2 + DC4_ag + N_B2 + BE3_75, family = binomial(link = "logit"), 
#     data = cleaned_data)


selected_data <- cleaned_data %>% select(danger, BO2_1 ,GS_mea_l_1 ,mh_stress ,T_NQ_FIR_P ,
LQ_1EQL ,L_BR ,BM1_4 ,BE5_1 ,L_LN ,DH6_dg ,DM1_dg ,
age ,educ ,DE2_dg ,BD1_11 ,N_PRG ,BM2_3 ,DC3_pr ,DE2_pt ,
LQ4_28 ,LQ4_03 ,DI1_2 ,marri_1 ,BD2_32 ,BE8_1 ,DJ8_pr ,
BA2_2_4 ,LK_LB_CO ,BH1 ,T_NQ_PH ,MH1_yr ,BP16_13 ,BS12_2 ,
BO3_04 ,BO3_10 ,BO3_09 ,BS5_34 ,BS5_33 ,LQ_5EQL ,DC6_pr ,
DC3_dg ,BS12_1 ,M_2_yr ,BE3_78 ,DC6_pt ,BP1 ,DI6_ag ,
DI6_dg ,DI2_2 ,GS_mea_l_3 ,D_1_1 ,DC1_dg ,DC1_ag ,BM13_4 ,
BP16_12 ,BP16_22 ,EC_wht_23 ,N_WAT_C ,BM2_5 ,DI6_pr ,
DM4_dg ,N_CHOL ,N_FAT ,DI1_pr ,BO3_12 ,DC3_ag ,apt_t ,
L_DN_FQ ,DN1_ag ,BS12_43 ,BE3_92 ,LQ4_15 ,LQ4_23 ,BS6_4_1 ,
BS6_4 ,BS6_2 ,DC4_ag ,N_B2 ,BE3_75)


####  multicollinearity(다중공선성) problem
# VIF 수식의 값이 10 이상 이면 해당 변수가 다중공선성이 존재하는 것으로 판단한다.

# 로지스틱 회귀 돌려본다
glm_fit <- glm(danger~.,family=binomial(link = 'logit'),data=selected_data)
summary(glm_fit)

vif(glm_fit)

# post_cleaned_data 의 경우 다중공선성이 10보다 큰 변수들이 존재하지 않음

####  imbalanced data problem

# 종속변수의 1, 0 개수를 샌다
table(selected_data$danger)
#    0    1 
# 6998  628 

# sampling
stratified_sampling <- strata(selected_data, stratanames = c("danger"), size =c(628,628),
                              method="srswor")

selected_data_sample <- getdata(selected_data, stratified_sampling)

# except useless results of sampling
selected_data_sample<-selected_data_sample%>%select(-ID_unit,-Prob,-Stratum)

table(selected_data_sample$danger)
#   0   1 
# 628 628 

dim(selected_data_sample)
# [1] 1256   67
# [1] 1256   80

table(is.na(selected_data_sample))

write.csv(selected_data_sample, file="/Users/seungyoungoh/workspace/classification_obesity_risk_groups/Data/LogisticRegression_Results/post_selected_data.csv", row.names=FALSE)