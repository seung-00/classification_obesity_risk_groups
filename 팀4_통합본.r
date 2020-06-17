if(TRUE)"
4팀의 코드는 integration.r, cleansing.r, 
feature_engineering_pre.r, feature_engineering_post.r, modeling.r,
DecisionTree2.r 로 구분됐습니다.
각 코드 시작 앞에 주석으로 해당 코드의 기능 및 결과를 설명했습니다.

feature_engineering_pre.r, feature_engineering_post.r는 전진선택 과정에서 컴퓨터 환경에 따라 긴 시간이 소요될 수 있습니다.
(제 경우에는 각각 5시간 정도 걸렸습니다)

각 코드의 입출력 경로를 환경에 맞춰 수정해주시기 바랍니다.
"







if(TRUE)"
여기서부터 다음 주석까지는 integration.r 코드입니다.
integration.r은 국민건강영양조사 2016~2018 데이터를 통합하는 기능을 합니다.
통합된 데이터는 
국민건강영양조사(2016~2018).csv
로 저장됩니다.
"
library(dplyr)

setwd("/Users/seungyoungoh/workspace/classification_obesity_risk_groups/Data")
df16 <- read.csv("국민건강영양조사(2016).csv", header = T,fileEncoding = "CP949")
df17 <- read.csv("국민건강영양조사(2017).csv", header = T)
df18 <- read.csv("국민건강영양조사(2018).csv", header = T)


df18_cols <- names(df18)
df17_cols <- names(df17)
df16_cols <- names(df16)
df17_18_cols <- c()
df16_17_18_cols <- c()


for(i in 1:length(df18_cols))
{   
    for(j in 1:length(df17_cols))
    if(df18_cols[i] == df17_cols[j])
    {
        df17_18_cols<-append(df17_18_cols, df18_cols[i])
        break
    }
}

for(i in 1:length(df17_18_cols))
{   
    for(j in 1:length(df16_cols))
    if(df17_18_cols[i] == df16_cols[j])
    {
        df16_17_18_cols<-append(df16_17_18_cols, df17_18_cols[i])
        break
    }
}


df18<-df18 %>% select(df16_17_18_cols)
df17<-df17 %>% select(df16_17_18_cols)
df16<-df16 %>% select(df16_17_18_cols)
df16_17_18<-rbind(df16,df18,df17)
dim(df16_17_18)
# 24269   690
write.csv(df16_17_18, file="국민건강영양조사(2016~2018).csv", row.names=FALSE)








if(TRUE)"
여기서부터 다음 주석까지는 cleansing.r 코드입니다.
cleansing.r은 국민건강영양조사(2016~2018).csv 를 전처리 후 '모름' 응답 값이 대체되지 않은 pre_cleaned_data.csv와, 대체된 post_cleaned_data.csv 로 저장합니다.
"

library(dplyr)
library(readr)
library(prettyR)

setwd("/Users/seungyoungoh/workspace/classification_obesity_risk_groups/Data")
#setwd("/.../classification_obesity_risk_groups/Data")
df16_17_18 <- read.csv("국민건강영양조사(2016~2018).csv", fileEncoding = "CP949")
dm_df <- df16_17_18
# 우리의 예측에 해당하지 않는, 체중 변화 여부를 무응답하거나 소아인 경우를 제외
dm_df <- dm_df %>% filter(dm_df$BO1_1 != 8 & dm_df$BO1_1 != 9)

dim(dm_df)
# [1] 18314   690
# 키나 몸무게가 nan 인 경우 제외
dm_df <- dm_df %>% filter(!is.na(dm_df$HE_wt))
dm_df <- dm_df %>% filter(!is.na(dm_df$HE_ht))
dim(dm_df)
# [1] 18253   690

# 과거 몸무게 col 생성
dm_df$past_wt <- dm_df$HE_wt
dm_df$past_wt <- ifelse(dm_df$BO1_3 == 1, dm_df$HE_wt - 4.5, dm_df$past_wt)
dm_df$past_wt <- ifelse(dm_df$BO1_3 == 2, dm_df$HE_wt - 8, dm_df$past_wt)
dm_df$past_wt <- ifelse(dm_df$BO1_3 == 3, dm_df$HE_wt - 12, dm_df$past_wt)

# 현재 or 과거 비만이 아니었던 행들만 선택 (현재 과거 모두 비만인 경우들을 제외)
dm_df <- dm_df %>% filter(((dm_df$HE_wt)/((dm_df$HE_ht/100)^2) <= 25) | ((dm_df$past_wt)/((dm_df$HE_ht/100)^2) <= 25))
dim(dm_df)
# 12890   691

# bmi 기준, if(현재 비만인데 과거에 비만이 아님) 1 else 0
dm_df$danger <- ifelse(((dm_df$HE_wt)/((dm_df$HE_ht/100)^2) > 25) &
((dm_df$past_wt)/((dm_df$HE_ht/100)^2) < 25), 1, 0)
table(dm_df$danger)
#    0    1 
# 11875  1015

# 필요없어진 past_wt 변수를 제거,
dm_df <- dm_df %>% select(-past_wt)

# 결측치가 많이 발견되어 검사에 무리를 준 특성을 제거
# 관련 없는 특성의 제거

dm_df <- dm_df %>% select(-age_month, - wt_pft, - wt_vt, - wt_nn, - wt_pfnt, - wt_pfvt, - wt_pfvtnt, - wt_vtnt, - wt_nnnt,
                          - BH9_14_1_01, - BH9_14_2_01, - BH9_14_3_01, - BH9_14_1_02, - BH9_14_2_02, - BH9_14_3_02,
                           - AC3_1_01, - AC3_2_01, - AC3_3_01, - AC8_1_01, - AC3_4_01, - AC8_2w_01,
                          - AC8_2_01, - AC8_3w_01, - AC8_3_01, - AC3_1_02, - AC3_2_02, - AC3_3_02, - AC8_1_02, - AC3_4_02,
                          - AC8_2w_02, - AC8_2_02, - AC8_3w_02, - AC8_3_02, - AC3_1_03, - AC3_2_03, - AC3_3_03, - AC8_1_03,
                          - AC3_4_03, - AC8_2w_03, - AC8_2_03, - AC8_3w_03, - AC8_3_03, - sc_seatblt, - sc_seatblt2, - LW_ms,
                          - LW_mp_a, - LW_ms_a, - LW_pr, - LW_pr_1, - LW_mt, - LW_mt_a1, - LW_mt_a2, - LW_br, - LW_br_ch,
                          - LW_br_dur, - LW_br_yy, - LW_br_mm, - LW_oc, - HE_dprg, - HE_mPLS, - HE_wt_pct, - HE_BMI_pct,
                          - HE_Folate, - HE_VitA, - HE_VitE, - HE_cough1, - HE_cough2, - HE_sput1, - HE_sput2,
                          - HE_PFTdr, - HE_PFTag, - HE_PFTtr, - HE_PFThs, - Y_BTH_WT, - Y_MTM_YN, - Y_MTM_S1, - Y_MTM_S2,
                          - Y_MTM_D1, - Y_MTM_D2, - Y_FM_YN, - Y_FM_S1, - Y_FM_S2, - Y_FM_D1, - Y_FM_D2, - Y_MLK_ST, - Y_WN_ST,
                          - Y_SUP_YN, - Y_SUP_KD1, - Y_SUP_KD3, - Y_SUP_KD4, - Y_SUP_KD7, - N_BFD_Y, - wt_hs) %>%
                          select(-HE_obe, - HE_HDL_st2, - HE_chol, - HE_HDL_st2, - HE_TG, - HE_LDL_drct, - HE_HCHOL, - HE_HTG, - HE_HBsAg,
                            - HE_ast, - HE_alt, - HE_hepaB, - ID, - BO1_3, - ID_fam, - LW_mp_e, - BO1_1, - HE_wc, - HE_wt,
                            - HE_BMI, - psu, - BO1_2, - BO1, - fam_rela, - region)

# 의미 없는 값이거나(예: 년도나 ID) 문자열 값이고 값들이 일치하지 않는 경우(예: BM14_2, 구강 진료를 받지 못한 상세 이유) 제거
dm_df <- dm_df %>% select(-X,-year, -mod_d, -DE1_35, -DC11_tp, -M_2_et, -BH9_14_4_02, -N_DT_DS, -N_DT_DS, -AC3_3e_01, -AC8_1e_01, -AC3_3e_02, -LQ4_24, -BH9_14_4_01, -N_DAY, -BM14_2, -BO3_11, -EC_wht_6, -BS5_31, -BS12_35)

src_data <- dm_df[,-grep("etc", names(dm_df))]
src_data <- dm_df[,-grep("ETC", names(dm_df))]
# etc 가 포함된 feature은 값에 부등호가 포함됨

cat("본래 데이터 열 개수: ",length(df16_17_18), ", 현재 데이터 열 개수:", length(src_data),"\n")


# 각 열의 결측치 개수를 센다.
# apply(src_data, 2, function(x) {sum(is.na(x))})

pre_cleaned_data <- src_data

for (i in 1:length(src_data))
{
  #결측치 > 2000 이면 해당 열을 제외한다.
  if(sum(is.na(src_data[i])) > 2000){
    pre_cleaned_data <- pre_cleaned_data %>% select(-names(src_data[i]))
  }
}

cat(length(src_data)-length(pre_cleaned_data), "개의 결측치가 2000개 이상인 열 제거\n")

cleaned_data <- na.omit(pre_cleaned_data)
cat(nrow(pre_cleaned_data)-nrow(cleaned_data), "개의 결측치가 포함된 행 제거\n")
# 3685 개의 결측치가 포함된 행 제거
dim(cleaned_data)
# [1] 7641  548
table(is.na(cleaned_data))
table(cleaned_data$danger)
#   0    1 
# 6998  628 

write.csv(cleaned_data, file="pre_cleaned_data.csv", row.names=FALSE)

post_cleaned_data <- cleaned_data

####  missing value treatment
# 모름: 9, 최빈값
colname_subset_mode1 <- list("cfam","genertn","house","live_t","ainc_unit1","marri_1","marri_2","npins","D_1_1","D_2_1","DI1_dg","DI1_pr","DI1_pt","DI1_2","DI2_dg","DI2_pr","DI2_pt","DI2_2","DI3_dg","DI3_pr","DI3_pt","DI3_2","DI5_dg","DI5_pr","DI5_pt","DI6_dg","DI6_pr","DI6_pt","DM2_dg","DM2_pr","DM2_pt","DM3_dg","DM3_pr","DM3_pt","DM4_dg","DM4_pr","DM4_pt","DJ2_dg","DJ2_pr","DJ2_pt","DJ4_dg","DJ4_pr","DJ4_pt","DJ4_3","DE1_dg","DE1_pr","DE1_pt","DE1_3","DE1_31","DE1_32","DE1_33","DE1_34","DE1_4","DE2_dg","DE2_pr","DE2_pt","DC1_dg","DC1_pr","DC1_pt","DC2_dg","DC2_pr","DC2_pt","DC3_dg","DC3_pr","DC3_pt","DC4_dg","DC4_pr","DC4_pt","DC5_dg","DC5_pr","DC5_pt","DC6_dg","DC6_pr","DC6_pt","DC7_dg","DC7_pr","DC7_pt","DC11_dg","DC11_pr","DC11_pt","DF2_dg","DF2_pr","DF2_pt","DL1_dg","DL1_pr","DL1_pt","DJ8_dg","DJ8_pr","DJ8_pt","DJ6_dg","DJ6_pr","DJ6_pt","DH4_dg","DH4_pr","DH4_pt","DH2_dg","DH2_pr","DH2_pt","DH3_dg","DH3_pr","DH3_pt","DH6_dg","DH6_pr","DH6_pt","DN1_dg","DN1_pr","DN1_pt","DK8_dg","DK8_pr","DK8_pt","DK9_dg","DK9_pr","DK9_pt","DK4_dg","DK4_pr","DK4_pt","DI9_yd","DF1_yd","DN6_yd","DJ9_yd","M_2_yr","BH9_11","BH1","BH2_61","LQ4_00","LQ4_01","LQ4_02","LQ4_03","LQ4_04","LQ4_05","LQ4_06","LQ4_07","LQ4_08","LQ4_09","LQ4_10","LQ4_11","LQ4_12","LQ4_13","LQ4_14","LQ4_15","LQ4_16","LQ4_21","LQ4_22","LQ4_25","LQ4_26","LQ4_27","LQ4_28","LQ4_29","LQ4_23","LQ4_17","LQ4_18","LQ4_19","LQ4_20","LQ1_sb","LQ2_ab","LQ_1EQL","LQ_2EQL","LQ_3EQL","LQ_4EQL","LQ_5EQL","AC1_yr","MH1_yr","MO1_wk","graduat","CH2_1","CH2_2","EC1_1","EC_stt_1","EC_stt_2","EC_wht_0","EC_lgw_4","EC_lgw_5","BO2_1","BO3_01","BO3_02","BO3_03","BO3_14","BO3_05","BO3_04","BO3_12","BO3_07","BO3_09","BO3_10","BD1","BD1_11","BD2_1","BD2_31","BD2_32","BD7_4","BD7_5","BA2_12","BA2_13","BA1_3","BA1_5","BA2_2_1","BA2_2_3","BA2_2_5","BA2_22","BA1_1","BA1_2","BP1","BP5","BP6_10","BP6_2","BP6_31","BP7","BS1_1","BS3_1","BS5","BS5_1","BS5_5","BS5_21","BS5_28","BS5_26","BS5_33","BS5_34","BS5_32","BS5_29","BS5_30","BS8_2","BS9_2","BS13","BS12_1","BS12_2","BS12_31","BS12_32","BS12_33","BS12_34","BS12_36","BS12_41","BS12_42","BS12_43","BS12_44","BS12_46","BS10_1","BE3_71","BE3_81","BE3_91","BE3_75","BE3_85","BE5_1","HE_fh","HE_HPfh1","HE_HPfh2","HE_HPfh3","HE_HLfh1","HE_HLfh2","HE_HLfh3","HE_IHDfh1","HE_IHDfh2","HE_IHDfh3","HE_STRfh1","HE_STRfh2","HE_STRfh3","HE_DMfh1","HE_DMfh2","HE_DMfh3","HE_THfh1","HE_THfh2","HE_THfh3","HE_HBfh1","HE_HBfh2","HE_HBfh3","BM1_0","BM1_1","BM1_2","BM1_3","BM1_4","BM1_5","BM1_6","BM1_7","BM1_8","BM2_1","BM2_3","BM2_2","BM2_4","BM2_5","BM13","BM13_1","BM13_2","BM13_3","BM13_4","BM13_5","BM7","BM8","OR1_2","MO4_00","MO4_4","MO4_9","MO4_7","MO4_8","MO4_17","MO4_11","MO4_12","MO4_18","MO4_15","BM14","T_Q_HR","T_Q_HR1","T_Q_VN","T_Q_VN1","T_NQ_PH","T_NQ_OCP","T_NQ_OCP_P","T_NQ_LS","T_NQ_FIR","T_NQ_FIR_P","GS_use","L_OUT_FQ","L_BR_TO","L_BR_WHO","L_LN_TO","L_LN_WHO","L_DN_TO","L_DN_WHO","LS_1YR","LK_EDU","LK_LB_CO","LK_LB_US","LK_LB_EF","N_DIET","N_DIET_WHY","LF_BUYER")

# 모름: 99, 최빈값
colname_subset_mode2 <- list("allownc","marri_2","tins","M_2_rs","BH9_13","educ","EC_occp","EC_wht_5","EC_lgw_2","EC_pedu_1","EC_pedu_2","BE3_31","BM14_1","LK_LB_IT")

# 모름: 9, 평균값
colname_subset_mean1 <- list("BE3_72", "BE3_82", "BE3_92", "BE3_76", "BE3_86")

# 모름: 99, 평균값
colname_subset_mean2 <- list("D_2_wk","LQ1_mn","LQ2_mn","AC3","MH1_1","MO1_1","BP16_11","BP16_12","BP16_13","BP16_14","BP16_21","BP16_22","BP16_23","BP16_24","BS3_3","BS6_2_1","BS6_2_2","BS6_4_1","BS6_4_2","BS10_2","BE3_73","BE3_74","BE3_83","BE3_84","BE3_93","BE3_94","BE3_77","BE3_78","BE3_87","BE3_88","BE8_1","BE8_2","BE3_32","BE3_33")

# 모름: 999, 평균값
colname_subset_mean3 <- list("DI1_ag","DI2_ag","DI3_ag","DI5_ag","DI6_ag","DM2_ag","DM3_ag","DM4_ag","DJ2_ag","DJ4_ag","DE1_ag","DE2_ag","DC1_ag","DC2_ag","DC3_ag","DC4_ag","DC5_ag","DC6_ag","DC7_ag","DC11_ag","DF2_ag","DL1_ag","DJ8_ag","DJ6_ag","DH4_ag","DH2_ag","DH3_ag","DH6_ag","DN1_ag","DK8_ag","DK9_ag","DK4_ag","DI9_ya","DF1_ya","DN6_ya","DJ9_ya","EC_wht_23","BD2","BA2_2_2","BA2_2_4","BA2_2_6","BS2_1","BS3_2","BS6_3","BS2","BS10_3","T_NQ_PH_T","T_NQ_OCP_T","T_NQ_LS_T")

# 모름: 9999, 평균값
colname_subset_mean4 <- list("Total_slp_wk", "Total_slp_wd","BS6_2","BS6_4")

# 모름: 999999, 평균값
colname_subset_mean5 <- list("ainc_1")

# 모름이 9인 설문조사 결과를 최빈값 대체
na_val <- 9
    for (i in 1:length(colname_subset_mode1))
        {
        col_name <- as.character(colname_subset_mode1[i])
        post_cleaned_data[col_name] <- apply(as.data.frame(post_cleaned_data[col_name]), 2, function(x) as.integer(gsub(na_val, Mode(x), x)))}

# 모름이 99인 설문조사 결과를 최빈값 대체
na_val <- 99
    for (i in 1:length(colname_subset_mode2))
        {
        col_name <- as.character(colname_subset_mode2[i])
        post_cleaned_data[col_name] <- apply(as.data.frame(post_cleaned_data[col_name]), 2, function(x) as.integer(gsub(na_val, Mode(x), x)))}

# 모름이 9인 설문조사 결과를 평균값 대체
na_val <- 9
for (i in 1:length(colname_subset_mean1))
{
    col_name <- as.character(colname_subset_mean1[i])
    post_cleaned_data[col_name] <- apply(as.data.frame(post_cleaned_data[col_name]), 2, function(x) as.integer(gsub(na_val, Mode(x), x)))
}

# 모름이 99인 설문조사 결과를 평균값 대체
na_val <- 99
    for (i in 1:length(colname_subset_mean2))
        {
        col_name <- as.character(colname_subset_mean2[i])
        post_cleaned_data[col_name] <- apply(as.data.frame(post_cleaned_data[col_name]), 2, function(x) as.integer(gsub(na_val, Mode(x), x)))}


# 모름이 999인 설문조사 결과를 평균값 대체
na_val <- 999
    for (i in 1:length(colname_subset_mean3))
        {
        col_name <- as.character(colname_subset_mean3[i])
        post_cleaned_data[col_name] <- apply(as.data.frame(post_cleaned_data[col_name]), 2, function(x) as.integer(gsub(na_val, Mode(x), x)))}

# 모름이 9999인 설문조사 결과를 평균값 대체
na_val <- 9999
    for (i in 1:length(colname_subset_mean4))
        {
        col_name <- as.character(colname_subset_mean4[i])
        post_cleaned_data[col_name] <- apply(as.data.frame(post_cleaned_data[col_name]), 2, function(x) as.integer(gsub(na_val, Mode(x), x)))}

# 모름이 999999인 설문조사 결과를 평균값 대체
na_val <- 999999
    for (i in 1:length(colname_subset_mean5))
        {
        col_name <- as.character(colname_subset_mean5[i])
        post_cleaned_data[col_name] <- apply(as.data.frame(post_cleaned_data[col_name]), 2, function(x) as.integer(gsub(na_val, Mode(x), x)))
}

table(is.na(post_cleaned_data))
# colSums(is.na(post_cleaned_data))  
dim(post_cleaned_data)
# [1] 7626  548

write.csv(post_cleaned_data, file="post_cleaned_data.csv", row.names=FALSE)









if(TRUE)"
여기서부터 다음 주석까지는 LogisticRegression/feature_engineering_pre.r 코드입니다.
feature_engineering_pre은 '모름'이 대체되지 않은 pre_cleaned_data.csv를 전처리 후 pre_selected_data.csv로 저장합니다
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









if(TRUE)"
여기서부터 다음 주석까지는 LogisticRegression/feature_engineering_post.r 코드입니다.
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









if(TRUE)"
여기서부터 다음 주석까지는 LogisticRegression/modeling.r 코드입니다.
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









if(TRUE)"
마지막으로 DecisionTree/DecisionTree2.R 코드입니다.
의사결정나무 역시 '모름'이 대체되지 않은 데이터와 대체되지 않은 데이터를 구분해서 모델링합니다.
'모름'이 대체되지 않은 데이터를 불러오는 부분은 주석처리 되어 있습니댜.
"

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