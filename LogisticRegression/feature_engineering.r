library(sampling)
library(car)
library(dplyr)
library(foreign)
library(caret)
library(ggplot2)
library(glmnet)
library(prettyR)



setwd("/Users/seungyoungoh/workspace/classification_obesity_risk_groups/Data")
cleaned_data <- read.csv("cleaned_data.csv", header=TRUE, stringsAsFactors = TRUE, na.strings=NA)

####  feature selection

# 팩터의 순서를 1,0으로 만들어 줘야함. 그래야 positive = 1, negative = 0 으로 glm이 인식함
# danger 팩터로


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


####  missing value treatment

selected_data <- cleaned_data <- cleaned_data%>% select(age , HE_Uacid , BO2_1 , BP_PHQ_5 , HE_dbp3 , 
    HE_DM , DI6_ag , BD7_64 , BA2_12 , BH1 , BP16_22 , LQ4_07 , 
    DJ2_dg , HE_Bplt , BA2_2_1 , BD7_5 , BS12_31 , O_chew_d , 
    HE_mens , GS_mea_l_1 , LQ4_05 , HE_Upro , L_DN_WHO , ainc_1 , 
    DH4_dg , L_BR_TO , HE_Ucot , DC3_dg , LK_EDU , BE8_2 , BS3_3 , 
    occp , HE_glu , LQ_4EQL , BS5_1 , EC_pedu_2 , T_Q_HR1 , DF2_pr , 
    DE1_pt , DE1_ag , HE_PLS , BM2_1 , N_VITC , DH3_ag , DC4_dg , 
    AC1_yr , MO4_11 , MO4_12 , HE_sbp1 , edu , BM8 , house , 
    DH3_pt , BS12_2 , DH4_ag , HE_HPfh1 , HE_Uph , N_MUFA , DF2_ag , BP_PHQ_8 , N_PROT , N_NIAC , live_t , LQ4_13, danger)

# 모름: 9
colname_subset1 <- list("BO2_1","BP_PHQ_5","BD7_64","BA2_12","BH1","LQ4_07","BD7_64","BA2_12","BH1","LQ4_07","DJ2_dg","BA2_2_1","BD7_5","BS12_31","LQ4_05","L_DN_WHO","DH4_dg","L_BR_TO","DC3_dg","LK_EDU","LQ_4EQL","BS5_1","T_Q_HR1","DF2_pr","DE1_pt","BM2_1","DC4_dg","AC1_yr","MO4_11","MO4_12","BM8","house","DH3_pt","BS12_2","HE_HPfh1","BP_PHQ_8","live_t","LQ4_13")

# 모름: 99
colname_subset2 <- list("BP16_22","BP16_22","BE8_2","BS3_3","EC_pedu_2")

# 모름: 999
colname_subset3 <- list("DI6_ag","DI6_ag","DE1_ag","DH3_ag","DH4_ag","DF2_ag")

# 모름: 999999 ainc_1

# 모름이 9인 설문조사 결과를 최빈값 대체
na_val <- 9
for (i in 1:length(colname_subset2))
{
    col_name <- as.character(colname_subset2[i])
    selected_data[col_name] <- apply(as.data.frame(selected_data[col_name]), 2, function(x) as.integer(gsub(na_val, Mode(x), x)))
}

# 모름이 99인 설문조사 결과를 최빈값 대체
na_val <- 99
for (i in 1:length(colname_subset2))
{
    col_name <- as.character(colname_subset2[i])
    selected_data[col_name] <- apply(as.data.frame(selected_data[col_name]), 2, function(x) as.integer(gsub(na_val, Mode(x), x)))
}

# 모름이 999인 설문조사 결과를 최빈값 대체
na_val <- 999
for (i in 1:length(colname_subset3))
{
    col_name <- as.character(colname_subset3[i])
    selected_data[col_name] <- apply(as.data.frame(selected_data[col_name]), 2, function(x) as.integer(gsub(na_val, Mode(x), x)))
}

# 모름: 999999인 설문조사 결과를 최빈값 대체
selected_data["ainc_1"] <- apply(as.data.frame(selected_data["ainc_1"]), 2, function(x) as.integer(gsub(na_val, Mode(x), x)))


table(is.na(selected_data))
dim(selected_data)

# [1] 2699   65
# 로지스틱 회귀 돌려본다
glm_fit <- glm(danger~.,data = selected_data, family = binomial(link = 'logit'))

####  multicollinearity(다중공선성) problem
# VIF 수식의 값이 10 이상 이면 해당 변수가 다중공선성이 존재하는 것으로 판단한다.

vif(glm_fit)
#         age    HE_Uacid       BO2_1    BP_PHQ_5     HE_dbp3       HE_DM 
#    2.896128    1.674702    1.104837    1.186391    2.299413    2.845215 
#      DI6_ag      BD7_64      BA2_12         BH1     BP16_22      LQ4_07 
#    1.015569    1.050265    1.396731    1.160192    1.037417 2821.131467 
#      DJ2_dg     HE_Bplt     BA2_2_1       BD7_5     BS12_31    O_chew_d 
#    1.016513    1.108738    1.103464    1.000004    1.060028    1.337961 
#     HE_mens  GS_mea_l_1      LQ4_05     HE_Upro    L_DN_WHO      ainc_1 
#    3.893324    3.164789 2358.953265    1.131044    1.127328    1.276165 
#      DH4_dg     L_BR_TO     HE_Ucot      DC3_dg      LK_EDU       BE8_2 
#   22.327465    1.268365    2.579397    1.000000    1.026491    1.031649 
#       BS3_3        occp      HE_glu     LQ_4EQL       BS5_1   EC_pedu_2 
#    1.328224    1.315437    2.631464    1.279522    2.961764    2.129319 
#     T_Q_HR1      DF2_pr      DE1_pt      DE1_ag      HE_PLS       BM2_1 
#    1.166597  210.309755 1372.532445 1373.196667    1.145302    1.163393 
#      N_VITC      DH3_ag      DC4_dg      AC1_yr      MO4_11      MO4_12 
#    1.057949   73.950212    1.016960    1.032374  164.399678  164.006320 
#     HE_sbp1         edu         BM8       house      DH3_pt      BS12_2 
#    2.520685    1.971696    1.357826    1.203329   73.703535    1.404940 
#      DH4_ag    HE_HPfh1      HE_Uph      N_MUFA      DF2_ag    BP_PHQ_8 
#   22.283937    1.069685    1.059878    1.991934  211.212132    1.228831 
#      N_PROT      N_NIAC      live_t      LQ4_13 
#    3.997048    3.505984    1.061098 2550.799553 

# LQ4_07 2821.131467 활동제한 사유: 당뇨병 -> 제거
# LQ4_05 2358.953265 활동제한 사유: 호흡 문제 -> 제거
# LQ4_13 2550.799553 활동제한 사유: 청각 문제
# DH4_dg 22.327465 중이염 의사진단 여부
# DH4_ag 22.283937 중이염 진단시기 -> 제거
# DE1_pt 1372.532445 당뇨병 치료
# DE1_ag 1373.196667 당뇨병 진단시기 -> 제거
# MO4_11 164.399678 진료항목: 발치 또는 구강내수술 -> 제거
# MO4_12 164.006320 진로항목: 다쳐서 빠지거나 부러진 치아 치료
# DH3_ag 73.950212 녹내장 진단시기 -> 제거
# DH3_pt 73.703535 녹내장 치료
# DF2_pr 210.309755 우울증 현재 유병 여부
# DF2_ag 211.212132 우울증 진단 시기 -> 제거

selected_data <- selected_data %>% select(-LQ4_07, -LQ4_05, -DH4_ag, -DE1_ag, -MO4_11, -DH3_ag, -DF2_ag)

dim(selected_data)
# [1] 2699   58

glm_fit2 <- glm(danger~.,data = selected_data, family = binomial(link = 'logit'))

vif(glm_fit2)
# age   HE_Uacid      BO2_1   BP_PHQ_5    HE_dbp3      HE_DM     DI6_ag 
# 2.906532   1.664304   1.100664   1.186024   2.281322   2.750362   1.013166 
# BD7_64     BA2_12        BH1    BP16_22     DJ2_dg    HE_Bplt    BA2_2_1 
# 1.048822   1.392409   1.153392   1.031837   1.013404   1.101723   1.099861 
# BD7_5    BS12_31   O_chew_d    HE_mens GS_mea_l_1    HE_Upro   L_DN_WHO 
# 1.000004   1.057483   1.335254   3.875835   3.156683   1.127459   1.123364 
# ainc_1     DH4_dg    L_BR_TO    HE_Ucot     DC3_dg     LK_EDU      BE8_2 
# 1.276658   1.047101   1.262729   2.575345   1.000000   1.028104   1.029108 
# BS3_3       occp     HE_glu    LQ_4EQL      BS5_1  EC_pedu_2    T_Q_HR1 
# 1.324365   1.314521   2.540000   1.282202   2.966949   2.130381   1.143182 
# DF2_pr     DE1_pt     HE_PLS      BM2_1     N_VITC     DC4_dg     AC1_yr 
# 1.139078   1.683707   1.137895   1.158937   1.057315   1.014023   1.025626 
# MO4_12    HE_sbp1        edu        BM8      house     DH3_pt     BS12_2 
# 1.053911   2.504689   1.971907   1.353739   1.200947   1.038792   1.406580 
# HE_HPfh1     HE_Uph     N_MUFA   BP_PHQ_8     N_PROT     N_NIAC     live_t 
# 1.052378   1.051543   1.986917   1.206423   3.983761   3.485590   1.057399 
# LQ4_13 
# 1.315353 


####  imbalanced data problem

# 종속변수의 1, 0 개수를 샌다
table(selected_data$danger)
#    0    1 
# 2212  487 


# sampling
stratified_sampling <- strata(selected_data, stratanames = c("danger"), size =c(480,480),
                              method="srswor")

selected_data <- getdata(selected_data, stratified_sampling)
dim(selected_data)
#[1] 960  61
str(selected_data)

# except useless results of sampling
selected_data<-selected_data%>%select(-ID_unit,-Prob,-Stratum)
dim(selected_data)
# [1] 960  58
table(is.na(selected_data))

write.csv(selected_data, file="selected_data.csv", row.names=FALSE)