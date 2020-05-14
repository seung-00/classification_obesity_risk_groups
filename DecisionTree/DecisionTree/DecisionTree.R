library(dplyr)
library(foreign)
library(caret)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(ROCR)

# R 3.6.3 사용

df18  <- read.spss("../../Data/Hn18_all.sav", header = T)

# 복사본 만들기
df18 <- data.frame(df18)
dm_df <- df18

# 데이터에서 위험군/정상군 분리
dm_df$is_obe <- ifelse(dm_df$HE_obe == 1 | dm_df$HE_obe == 2, 0, 1) # dataframe에 새로운 column을 추가하는 코드

# 우리의 예측에 해당하지 않는, 체중 변화 여부를 무응답하거나 소아인 경우를 제외
# 콜레스테롤과 같은 비만자의 현재 신체 건강을 나타내는 수치가 기준이 되지 않도록 비만인 사람들만 고려
dm_df <- dm_df %>% filter(dm_df$BO1_1 != 8 & dm_df$BO1_1 != 9)
dm_df <- filter(dm_df, dm_df$is_obe == 1)

# danger가 No이면 정상, Yes이면 위험
dm_df$danger <- ifelse(dm_df$is_obe == 1 & dm_df$BO1_1 == 3, "Yes", "No")

# 필요없어진 is_obe 변수를 제거,
# danger가 NA(결측)인 데이터를 제거
dm_df <- dm_df %>% dplyr::select(-is_obe) %>%
    dplyr::filter(!is.na(dm_df$danger))

# 결측치가 많이 발견되어 검사에 무리를 준 특성을 제거
# 관련 없는 특성의 제거
dm_df <- dm_df %>% select(-age_month, - wt_pft, - wt_vt, - wt_nn, - wt_pfnt, - wt_pfvt, - wt_pfvtnt, - wt_vtnt, - wt_nnnt, -wt_tot, 
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
                            - HE_BMI, - psu, - BO1_2, - BO1, - fam_rela, - region, - N_DT_DS)

#View(dm_df)



# 셋을 Yes와 No로 분리
train_no <- dm_df %>% filter(dm_df$danger == "No")
train_yes <- dm_df %>% filter(dm_df$danger == "Yes")

# Train 셋을 리샘플링(Yes와 No를 1:1 비율로)
train_no_ran_sam <- sample(1:nrow(train_no), nrow(train_yes))
train_no <- train_no[train_no_ran_sam,]
dm_df <- rbind(train_no, train_yes)

# test, training셋 분리
set.seed(1000)
intrain <- createDataPartition(y=dm_df$danger, p = 0.8, list = F)
train <- dm_df[intrain,]
test <- dm_df[-intrain,]



# tree 그리기
t <- rpart(danger ~ ., data = train, method = 'class', control = rpart.control(minsplit = 2, minbucket = 1, cp = 0.0059))


# 모든 cp를 돌며 test 셋에 대한 confusion matrix를 구한다
for (i in 1:nrow(t$cptable)) {
    print(i, t$cptable[i, "CP"])
    ptree <- prune(t, cp = t$cptable[i, "CP"])

    pred <- predict(ptree, test, type = "class")
    print(confusionMatrix(pred, as.factor(test$danger), positive = "Yes"))

    pred <- predict(ptree, test, type = "prob")
    pred <- prediction(pred[, 2], test$danger)
    prf <- performance(pred, "tpr", "fpr")
    win.graph()
    plot(prf)
    abline(0, 1, lty = 2)
    print(performance(pred, "auc"))
}

# 트리 가지치기
ptree <- prune(t, cp = t$cptable[which.min(t$cptable[, "xerror"]), "CP"])

fancyRpartPlot(ptree)