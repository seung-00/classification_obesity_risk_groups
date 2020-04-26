library(foreign)
df18  <- read.spss("../../Data/Hn18_all.sav", header = T)

#View(df)

low <- df18$HE_obe == 1

# 복사본 만들기
df_t <- df18

df18$is_obe <- ifelse(df18$HE_obe == 1 | df18$HE_obe == 2, 0, 1)
df18$danger <- ifelse(df18$is_obe == 1 & df18$BO1_1 == 3, 1, 0)

View(df18)