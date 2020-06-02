library(dplyr)
library(readr)

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
dm_df$was_kg <- dm_df$HE_wt
dm_df$was_kg <- ifelse(dm_df$BO1_3 == 1, dm_df$HE_wt - 4.5, dm_df$was_kg)
dm_df$was_kg <- ifelse(dm_df$BO1_3 == 2, dm_df$HE_wt - 8, dm_df$was_kg)
dm_df$was_kg <- ifelse(dm_df$BO1_3 == 3, dm_df$HE_wt - 12, dm_df$was_kg)

# 현재 or 과거 비만이 아니었던 행들만 선택 (현재 과거 모두 비만인 경우들을 제외)
dm_df <- dm_df %>% filter(((dm_df$HE_wt)/((dm_df$HE_ht/100)^2) <= 25) | ((dm_df$was_kg)/((dm_df$HE_ht/100)^2) <= 25))
dim(dm_df)
# 12890   691

# bmi 기준, if(현재 비만인데 과거에 비만이 아님) 1 else 0
dm_df$danger <- ifelse(((dm_df$HE_wt)/((dm_df$HE_ht/100)^2) > 25) &
((dm_df$was_kg)/((dm_df$HE_ht/100)^2) < 25), 1, 0)
table(dm_df$danger)
#    0    1 
# 11875  1015

# 필요없어진 was_kg 변수를 제거,
dm_df <- dm_df %>% select(-was_kg)

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
#    0    1 
# 7013  628

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
        selected_data[col_name] <- apply(as.data.frame(selected_data[col_name]), 2, function(x) as.integer(gsub(na_val, Mode(x), x)))}

# 모름이 99인 설문조사 결과를 최빈값 대체
na_val <- 99
    for (i in 1:length(colname_subset_mode2))
        {
        col_name <- as.character(colname_subset_mode2[i])
        selected_data[col_name] <- apply(as.data.frame(selected_data[col_name]), 2, function(x) as.integer(gsub(na_val, Mode(x), x)))}

# 모름이 9인 설문조사 결과를 평균값 대체
na_val <- 9
for (i in 1:length(colname_subset_mean1))
{
    col_name <- as.character(colname_subset_mean1[i])
    selected_data[col_name] <- apply(as.data.frame(selected_data[col_name]), 2, function(x) as.integer(gsub(na_val, Mode(x), x)))
}

# 모름이 99인 설문조사 결과를 평균값 대체
na_val <- 99
    for (i in 1:length(colname_subset_mean2))
        {
        col_name <- as.character(colname_subset_mean2[i])
        selected_data[col_name] <- apply(as.data.frame(selected_data[col_name]), 2, function(x) as.integer(gsub(na_val, Mode(x), x)))}


# 모름이 999인 설문조사 결과를 평균값 대체
na_val <- 999
    for (i in 1:length(colname_subset_mean3))
        {
        col_name <- as.character(colname_subset_mean3[i])
        selected_data[col_name] <- apply(as.data.frame(selected_data[col_name]), 2, function(x) as.integer(gsub(na_val, Mode(x), x)))}

# 모름이 9999인 설문조사 결과를 평균값 대체
na_val <- 9999
    for (i in 1:length(colname_subset_mean4))
        {
        col_name <- as.character(colname_subset_mean4[i])
        selected_data[col_name] <- apply(as.data.frame(selected_data[col_name]), 2, function(x) as.integer(gsub(na_val, Mode(x), x)))
}

# 모름이 999999인 설문조사 결과를 평균값 대체
na_val <- 999999
    for (i in 1:length(colname_subset_mean5))
        {
        col_name <- as.character(colname_subset_mean5[i])
        selected_data[col_name] <- apply(as.data.frame(selected_data[col_name]), 2, function(x) as.integer(gsub(na_val, Mode(x), x)))
}

table(is.na(cleaned_data))
# colSums(is.na(cleaned_data))  
dim(cleaned_data)


write.csv(cleaned_data, file="cleaned_data.csv", row.names=FALSE)