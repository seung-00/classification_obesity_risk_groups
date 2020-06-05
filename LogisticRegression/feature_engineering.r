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
cleaned_data <- read_csv("cleaned_data.csv")
unknown_value <- read_csv("결측치(모름)_조사(unf).csv")
unknown_value <- unknown_value %>% select(value_name, type)

col_name <-as.character(unknown_value[1,1])

cleaned_data[,col_name]<-as.factor(cleaned_data[,col_name])

str(cleaned_data$town_t)

cleaned_data[col_name] <- apply(as.data.frame(cleaned_data[col_name]), 2, as.factor)


cleaned_data$town_t <- as.factor(cleaned_data$town_t)

for (i in 1:length(unknown_value))
{
    if(unknown_value[i,2] == 1 || unknown_value[i,2] == 3)
    {
        unknown_value_name <-as.character(unknown_value[1,1])
        unknown_value_name <- unknown_value[i,1]   
    }
}
state1 <- dim(cleaned_data)
cleaned_data$danger <- as.factor(cleaned_data$danger)



####  feature selection

# 팩터의 순서를 1,0으로 만들어 줘야함. 그래야 positive = 1, negative = 0 으로 glm이 인식함
# danger 팩터로

table(is.na(cleaned_data))


# 전진 선택
Model_full <- glm(danger~.,family=binomial(link = 'logit'),data=cleaned_data)
Model_non <- glm(danger~1,family=binomial(link = 'logit'),data=cleaned_data)


Model_forward <- step(Model_non,list(lower=Model_non,upper=Model_full),direction = "forward")
summary(Model_forward)


# 전진 선택 결과
# glm(formula = danger ~ age , sex , BP_PHQ_5 , L_DN , BS3_3 , 
#     BS12_31 , BD7_5 , BA1_3 , N_VA_RAE , DH2_dg , BP_PHQ_6 , 
#     DI1_pt , DJ2_dg , DI6_pt , DI6_ag , DI3_2 , allownc , BA2_13 , 
#     DC3_ag , HE_Uket , HE_Uph , ainc_1 , BE3_33 , HE_DMfh3 , 
#     HE_Uro , DH6_ag , DH6_pt , BM13_1 , BE3_31 , BD2_14 , HE_Upro , 
#     DE1_pt , DE1_pr , BP7 , BP16_23 , BP16_21 , BP_PHQ_7 , T_Q_HR1 , 
#     DE2_dg , npins , OR1_2 , edu , EC_pedu_2 , HE_hsCRP , DC2_dg , 
#     HE_UCREA , LQ2_mn , BA2_2_4 , Total_slp_wd , BP16_22 , BP_PHQ_9 , 
#     BA2_22 , HE_DMdg , BE3_76 , BE3_78 , BH1 , BM2_2 , DJ8_pt , 
#     DJ8_pr , L_BR_WHO , DF2_pr , DI1_2 , mh_stress , BE8_2 , 
#     BP2 , T_NQ_OCP_T , HE_fst, family = binomial(link = "logit"), 
#     data = cleaned_data)


####  multicollinearity(다중공선성) problem
# VIF 수식의 값이 10 이상 이면 해당 변수가 다중공선성이 존재하는 것으로 판단한다.

# 로지스틱 회귀 돌려본다
glm_fit <- glm(danger~.,data = selected_data, family = binomial(link = 'logit'))


vif(glm_fit)

#          age          sex     BP_PHQ_5         L_DN        BS3_3      BS12_31 
# 3.330541e+00 1.610554e+00 1.280987e+00 1.061425e+00 1.052161e+00 1.055157e+00 
#        BD7_5        BA1_3     N_VA_RAE       DH2_dg     BP_PHQ_6       DI1_pt 
# 9.999993e-01 1.117827e+00 1.075456e+00 1.274931e+00 1.664840e+00 2.666156e+01 
#       DJ2_dg       DI6_pt       DI6_ag        DI3_2      allownc       BA2_13 
# 1.044233e+00 8.687129e+08 8.687157e+08 1.141833e+00 1.154925e+00 1.108293e+00 
#       DC3_ag      HE_Uket       HE_Uph       ainc_1       BE3_33     HE_DMfh3 
# 9.999999e-01 1.072796e+00 1.128907e+00 1.204300e+00 1.729595e+00 1.046702e+00 
#       HE_Uro       DH6_ag       DH6_pt       BM13_1       BE3_31       BD2_14 
# 1.037144e+00 2.384905e+02 2.384905e+02 1.087392e+00 1.791750e+00 1.209803e+00 
#      HE_Upro       DE1_pt       DE1_pr          BP7      BP16_23      BP16_21 
# 1.265342e+00 9.702801e+09 9.702801e+09 1.321742e+00 3.551216e+00 1.919804e+00 
#     BP_PHQ_7      T_Q_HR1       DE2_dg        npins        OR1_2          edu 
# 1.155254e+00 1.142967e+00 1.104230e+00 1.225817e+00 1.160285e+00 1.970023e+00 
#    EC_pedu_2     HE_hsCRP       DC2_dg     HE_UCREA       LQ2_mn      BA2_2_4 
# 1.832458e+00 1.057927e+00 1.000000e+00 1.697498e+00 1.066598e+00 1.081784e+00 
# Total_slp_wd      BP16_22     BP_PHQ_9       BA2_22      HE_DMdg       BE3_76 
# 2.019039e+00 1.059128e+00 1.663049e+00 1.107452e+00 4.799423e+01 6.970761e+00 
#       BE3_78          BH1        BM2_2       DJ8_pt       DJ8_pr     L_BR_WHO 
# 6.904309e+00 1.147717e+00 1.149127e+00 2.009978e+02 2.004095e+02 1.157595e+00 
#       DF2_pr        DI1_2    mh_stress        BE8_2          BP2   T_NQ_OCP_T 
# 1.266729e+00 2.708442e+01 1.240109e+00 1.039885e+00 1.193954e+00 1.094660e+00 
#       HE_fst 
# 1.075512e+00 


# DI1_pt	2.67E+01	고혈압 치료
# DI1_2     2.71E+01	혈압조절제 복용 -> 제거
# DI6_pt	8.69E+08	협심증 치료
# DI6_ag	8.69E+08	협심증 진단시기 -> 제거
# DH6_pt	2.38E+02	황반변성 치료
# DH6_ag	2.38E+02	황반변성 진단시기 -> 제거
# DE1_pt	9.70E+09	당뇨병 치료
# DE1_pr	9.70E+09	당뇨병현재유병여부 -> 제거
# HE_DMdg	4.80E+01	당뇨병 의사진단 여부 -> 제거
# DJ8_pt	2.01E+02	알레르기비염 치료
# DJ8_pr	2.00E+02	알레르기비염 현재 유병 여부 -> 제거

selected_data <- selected_data %>% select(-DI1_2, -DI6_ag, -DH6_ag, -DE1_pr, -HE_DMdg, -DJ8_pr)


dim(selected_data)
# [1] 1614   62


glm_fit2 <- glm(danger~.,data = selected_data, family = binomial(link = 'logit'))

vif(glm_fit2)
#      age          sex     BP_PHQ_5         L_DN        BS3_3      BS12_31 
# 3.338492     1.599755     1.281979     1.059849     1.052566     1.036126 
#    BD7_5        BA1_3     N_VA_RAE       DH2_dg     BP_PHQ_6       DI1_pt 
# 1.000005     1.114409     1.072869     1.292253     1.657380     1.395050 
#   DJ2_dg       DI6_pt        DI3_2      allownc       BA2_13       DC3_ag 
# 1.042961     1.041365     1.154145     1.138021     1.112802     1.000000 
#  HE_Uket       HE_Uph       ainc_1       BE3_33     HE_DMfh3       HE_Uro 
# 1.069123     1.124241     1.195174     1.741238     1.043940     1.036578 
#   DH6_pt       BM13_1       BE3_31       BD2_14      HE_Upro       DE1_pt 
# 1.081742     1.082691     1.804948     1.203066     1.266906     1.204403 
#      BP7      BP16_23      BP16_21     BP_PHQ_7      T_Q_HR1       DE2_dg 
# 1.325023     3.540227     1.901976     1.150187     1.141445     1.090498 
#    npins        OR1_2          edu    EC_pedu_2     HE_hsCRP       DC2_dg 
# 1.226606     1.160095     1.954188     1.834980     1.065662     1.000000 
# HE_UCREA       LQ2_mn      BA2_2_4 Total_slp_wd      BP16_22     BP_PHQ_9 
# 1.703174     1.059472     1.079963     2.007131     1.061109     1.620799 
#   BA2_22       BE3_76       BE3_78          BH1        BM2_2       DJ8_pt 
# 1.106038     6.939122     6.880432     1.147442     1.149798     1.094493 
# L_BR_WHO       DF2_pr    mh_stress        BE8_2          BP2   T_NQ_OCP_T 
# 1.141871     1.272977     1.245535     1.040722     1.209137     1.087167 
#   HE_fst 
# 1.073204 


####  imbalanced data problem

# 종속변수의 1, 0 개수를 샌다
table(selected_data$danger)
#    0    1 
#  487 1127 

# sampling
stratified_sampling <- strata(selected_data, stratanames = c("danger"), size =c(480,480),
                              method="srswor")

selected_data <- getdata(selected_data, stratified_sampling)

str(selected_data)
# except useless results of sampling
selected_data<-selected_data%>%select(-ID_unit,-Prob,-Stratum)

table(selected_data$danger)
#   0   1 
# 480 480 

dim(selected_data)
# [1] 960  58

table(is.na(selected_data))

write.csv(selected_data, file="selected_data.csv", row.names=FALSE)