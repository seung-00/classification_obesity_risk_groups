library(dplyr)

setwd("/Users/seungyoungoh/workspace/classification_obesity_risk_groups/Data")
#setwd("/.../classification_obesity_risk_groups/Data")
df18 <- read.csv("국민건강영양조사(2018).csv", header = T)
dm_df <- df18
# 우리의 예측에 해당하지 않는, 체중 변화 여부를 무응답하거나 소아인 경우를 제외
dm_df <- dm_df %>% filter(dm_df$BO1_1 != 8 & dm_df$BO1_1 != 9)

dim(dm_df)
# [1] 6180  737
# 키나 몸무게가 nan 인 경우 제외
dm_df <- dm_df %>% filter(!is.na(dm_df$HE_wt))
dm_df <- dm_df %>% filter(!is.na(dm_df$HE_ht))
dim(dm_df)
# [1] 6137  737

# 과거 몸무게 col 생성
dm_df$was_kg <- dm_df$HE_wt
dm_df$was_kg <- ifelse(dm_df$BO1_3 == 1, dm_df$HE_wt - 4.5, dm_df$was_kg)
dm_df$was_kg <- ifelse(dm_df$BO1_3 == 2, dm_df$HE_wt - 8, dm_df$was_kg)
dm_df$was_kg <- ifelse(dm_df$BO1_3 == 3, dm_df$HE_wt - 12, dm_df$was_kg)

# 현재 or 과거 비만이 아니었던 행들만 선택 (현재 과거 모두 비만인 경우들을 제외)
dm_df <- dm_df %>% filter(((dm_df$HE_wt)/((dm_df$HE_ht/100)^2) <= 25) | ((dm_df$was_kg)/((dm_df$HE_ht/100)^2) <= 25))
dim(dm_df)
# [1] 4326  738

# bmi 기준, if(현재 비만인데 과거에 비만이 아님) 1 else 0
dm_df$danger <- ifelse(((dm_df$HE_wt)/((dm_df$HE_ht/100)^2) > 25) &
((dm_df$was_kg)/((dm_df$HE_ht/100)^2) < 25), 1, 0)
table(dm_df$danger)
#    0    1 
# 4008  318 

# 필요없어진 was_kg 변수를 제거,
dm_df <- dm_df %>% select(-was_kg)

# 결측치가 많이 발견되어 검사에 무리를 준 특성을 제거
# 관련 없는 특성의 제거
dm_df <- dm_df %>% select(-age_month, - wt_pft, - wt_vt, - wt_nn, - wt_pfnt, - wt_pfvt, - wt_pfvtnt, - wt_vtnt, - wt_nnnt,
                          - BH9_14_1_01, - BH9_14_2_01, - BH9_14_3_01, - BH9_14_1_02, - BH9_14_2_02, - BH9_14_3_02, - BH9_14_1_03,
                          - BH9_14_2_03, - BH9_14_3_03, - AC3_1_01, - AC3_2_01, - AC3_3_01, - AC8_1_01, - AC3_4_01, - AC8_2w_01,
                          - AC8_2_01, - AC8_3w_01, - AC8_3_01, - AC3_1_02, - AC3_2_02, - AC3_3_02, - AC8_1_02, - AC3_4_02,
                          - AC8_2w_02, - AC8_2_02, - AC8_3w_02, - AC8_3_02, - AC3_1_03, - AC3_2_03, - AC3_3_03, - AC8_1_03,
                          - AC3_4_03, - AC8_2w_03, - AC8_2_03, - AC8_3w_03, - AC8_3_03, - sc_seatblt, - sc_seatblt2, - LW_ms,
                          - LW_mp_a, - LW_ms_a, - LW_pr, - LW_pr_1, - LW_mt, - LW_mt_a1, - LW_mt_a2, - LW_br, - LW_br_ch,
                          - LW_br_dur, - LW_br_yy, - LW_br_mm, - LW_oc, - HE_dprg, - HE_mPLS, - HE_wt_pct, - HE_BMI_pct,
                          - HE_Folate, - HE_VitA, - HE_VitE, - HE_NNAL, - HE_cough1, - HE_cough2, - HE_sput1, - HE_sput2,
                          - HE_PFTdr, - HE_PFTag, - HE_PFTtr, - HE_PFThs, - Y_BTH_WT, - Y_MTM_YN, - Y_MTM_S1, - Y_MTM_S2,
                          - Y_MTM_D1, - Y_MTM_D2, - Y_FM_YN, - Y_FM_S1, - Y_FM_S2, - Y_FM_D1, - Y_FM_D2, - Y_MLK_ST, - Y_WN_ST,
                          - Y_SUP_YN, - Y_SUP_KD1, - Y_SUP_KD3, - Y_SUP_KD4, - Y_SUP_KD7, - N_BFD_Y, - wt_hs) %>%
                          select(-HE_obe, - HE_HDL_st2, - HE_chol, - HE_HDL_st2, - HE_TG, - HE_LDL_drct, - HE_HCHOL, - HE_HTG, - HE_HBsAg,
                            - HE_ast, - HE_alt, - HE_hepaB, - ID, - BO1_3, - ID_fam, - id_M, - id_F, - LW_mp_e, - BO1_1, - HE_wc, - HE_wt,
                            - HE_BMI, - psu, - BO1_2, - BO1, - fam_rela, - region)

# 의미 없는 값이거나(예: 년도나 ID) 문자열 값이고 값들이 일치하지 않는 경우(예: BM14_2, 구강 진료를 받지 못한 상세 이유) 제거
src_data <- dm_df %>% select(-X,-year, -mod_d, -DE1_35, -DC11_tp, -DC12_tp, -M_2_et, -BH9_14_4_02, -N_DT_DS, -N_DT_DS, -AC3_3e_01, -AC8_1e_01, -AC3_3e_02, -LQ4_24, -BH9_14_4_01, -N_DAY, -BM14_2, -BP2_2, -BO3_11, -EC_wht_6, -BS5_31, -BP2_2, -BD7_67, -BS12_35)

src_data <- src_data[,-grep("etc", names(src_data))]
src_data <- src_data[,-grep("ETC", names(src_data))]
# etc 가 포함된 feature은 값에 부등호가 포함됨

cat("본래 데이터 열 개수: ",length(df18), ", 현재 데이터 열 개수:", length(src_data),"\n")


# 각 열의 결측치 개수를 센다.
# apply(src_data, 2, function(x) {sum(is.na(x))})

pre_cleaned_data <- src_data

for (i in 1:length(src_data))
{
  #결측치 > 2000 이면 해당 열을 제외한다.
  if(sum(is.na(src_data[i])) > 2000){
    pre_cleaned_data <- pre_cleaned_data %>% select(-names(src_data[i]))
    
    # print(names(pre_cleaned_data[i]))
    # "BH9_14_4_03" 인플루엔자 상세
    # "AC8_1e_02" 손상 치료 기타 상세
    # "AC3_3e_03" 손상 치료 기타 상세
    # "AC8_1e_03" 손상 치료 기타 상세
    # "BS12_45" 현재 사용 담배 종류
    # "BM13_6" 치아 손상 사유
  }
}

cat(length(src_data)-length(pre_cleaned_data), "개의 결측치가 2000개 이상인 열 제거\n")

# 만약 결측치 1000 이상인 경우를 살펴보면 다음과 같은 열들이 추가 선택된다.
#  "BH1" 건강 검진 수진 여부 
#  "MH1_yr" 1년간 입원 이용 여부
#  "MH1_1"  입원 이용 횟수 
#  "MO1_wk" 2주간 외래 이용 여부
#  "BS10_3" 최근 1달 동안 담배를 피운 날 하루 평균 몇 개비를 피웠나
#  "HE_UNa_etc" 요나트륨 장비분석범위
#  "MO4_4" 진료항목: 구강검사?


cleaned_data <- na.omit(pre_cleaned_data)
cat(nrow(pre_cleaned_data)-nrow(cleaned_data), "개의 결측치가 포함된 행 제거\n")
# 2464 개의 결측치가 포함된 행 제거
dim(cleaned_data)
# [1] 1862  578
table(cleaned_data$danger)
#    0    1 
# 1706  156 

write.csv(cleaned_data, file="cleaned_data2.csv", row.names=FALSE)