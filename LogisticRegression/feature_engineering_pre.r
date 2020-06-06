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
cleaned_data <- read.csv("pre_cleaned_data.csv")

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

# 전진 선택 결과 1
# glm(formula = danger ~ BO2_1 + GS_mea_l_1 + mh_stress + T_NQ_FIR_P + 
#     LQ_1EQL + L_BR + DJ4_3 + BM1_4 + BE5_1 + DE2_pt + L_LN + 
#     DH6_dg + DM1_dg + age + LQ4_28 + D_1_1 + DI1_pt + graduat + 
#     EC_lgw_2 + DC3_pr + LQ_5EQL + BM2_3 + BD2_32 + marri_1 + 
#     BA2_2_4 + DJ8_dg + LQ4_15 + MH1_yr + BE8_2 + BH1 + T_NQ_PH + 
#     DC6_dg + BO3_04 + BO3_10 + BO3_09 + BS5_34 + BS5_33 + BS12_2 + 
#     dr_month + N_PRG + BM2_5 + EC_wht_0 + LQ4_27 + BP1 + BO3_12 + 
#     ainc_1 + LK_LB_CO + BP16_12 + BP16_22 + BS12_1 + BM13_4 + 
#     house + DI4_dg + DI5_dg + DI5_ag + DK4_pt + apt_t + LQ4_29 + 
#     L_DN_FQ + DI6_dg + DI6_ag + DI5_pr + N_DIET + N_DIET_WHY + 
#     DI2_ag + BP16_14 + DM4_dg + BE8_1 + GS_mea_l_3 + BD1_11 + 
#     DC7_pr + DC1_dg + DC1_ag + BS12_31 + EC_wht_23 + N_WAT_C + 
#     BE3_78 + BE3_75 + BS6_4_1 + BS6_4 + BS6_2 + BS3_3 + LQ4_02 + 
#     BO3_01 + BM1_8 + BE3_92 + BA2_2_5 + DC3_ag, family = binomial(link = "logit"), 
#     data = cleaned_data)     

selected_data_pre <- cleaned_data %>%select(danger,BO2_1 , GS_mea_l_1 , mh_stress , T_NQ_FIR_P , 
    LQ_1EQL , L_BR , DJ4_3 , BM1_4 , BE5_1 , DE2_pt , L_LN , 
    DH6_dg , DM1_dg , age , LQ4_28 , D_1_1 , DI1_pt , graduat , 
    EC_lgw_2 , DC3_pr , LQ_5EQL , BM2_3 , BD2_32 , marri_1 , 
    BA2_2_4 , DJ8_dg , LQ4_15 , MH1_yr , BE8_2 , BH1 , T_NQ_PH , 
    DC6_dg , BO3_04 , BO3_10 , BO3_09 , BS5_34 , BS5_33 , BS12_2 , 
    dr_month , N_PRG , BM2_5 , EC_wht_0 , LQ4_27 , BP1 , BO3_12 , 
    ainc_1 , LK_LB_CO , BP16_12 , BP16_22 , BS12_1 , BM13_4 , 
    house , DI4_dg , DI5_dg , DI5_ag , DK4_pt , apt_t , LQ4_29 , 
    L_DN_FQ , DI6_dg , DI6_ag , DI5_pr , N_DIET , N_DIET_WHY , 
    DI2_ag , BP16_14 , DM4_dg , BE8_1 , GS_mea_l_3 , BD1_11 , 
    DC7_pr , DC1_dg , DC1_ag , BS12_31 , EC_wht_23 , N_WAT_C , 
    BE3_78 , BE3_75 , BS6_4_1 , BS6_4 , BS6_2 , BS3_3 , LQ4_02 , 
    BO3_01 , BM1_8 , BE3_92 , BA2_2_5 , DC3_ag)

####  multicollinearity(다중공선성) problem
# VIF 수식의 값이 10 이상 이면 해당 변수가 다중공선성이 존재하는 것으로 판단한다.

# 로지스틱 회귀 돌려본다
glm_fit <- glm(danger~.,family=binomial(link = 'logit'),data=selected_data_pre)
summary(glm_fit)
vif(glm_fit)

selected_data_pre <- selected_data_pre %>% select(-DC1_ag, -DI6_ag, -DI5_ag, -LQ4_27, -BS6_4, -BO3_10, -DC3_pr, -BS5_34, -BO3_09, -DI5_pr, -BO3_04, -BS12_2, -N_DIET_WHY, -BE3_78, -DI4_dg, -GS_mea_l_3)

glm_fit2 <- glm(danger~.,data = selected_data_pre, family = binomial(link = 'logit'))
vif(glm_fit2)

selected_data_pre <- selected_data_pre %>% select(-LQ4_28, -BO3_12, -BO3_01, -LQ4_29, -BS6_4_1, -LQ4_02)

glm_fit3 <- glm(danger~.,data = selected_data_pre, family = binomial(link = 'logit'))
vif(glm_fit3)

# 다중공선성 제거 완료


####  imbalanced data problem

# 종속변수의 1, 0 개수를 샌다
table(selected_data_pre$danger)
#    0    1 
# 6998  628 

# sampling
stratified_sampling <- strata(selected_data_pre, stratanames = c("danger"), size =c(628,628),
                              method="srswor")

selected_data_sample <- getdata(selected_data_pre, stratified_sampling)

# except useless results of sampling
selected_data_sample<-selected_data_sample%>%select(-ID_unit,-Prob,-Stratum)

table(selected_data_sample$danger)
#   0   1 
# 628 628 

dim(selected_data_sample)
# [1] 1256   67
# [1] 1256   80

table(is.na(selected_data_sample))

write.csv(selected_data_sample, file="/Users/seungyoungoh/workspace/classification_obesity_risk_groups/Data/LogisticRegression_Results/pre_selected_data.csv", row.names=FALSE)
