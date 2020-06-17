if(TRUE)"
integration.r 코드입니다.
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