library(dplyr)

setwd("/Users/seungyoungoh/workspace/classification_obesity_risk_groups/Data")
df18 <- read.csv("국민건강영양조사(2017).csv", header = T)
df17 <- read.csv("국민건강영양조사(2018).csv", header = T)

df18_cols <- names(df18)
df17_cols <- names(df17)
df17_18_cols <- c()

for(i in 1:length(df18_cols))
{   
    for(j in 1:length(df17_cols))
    if(df18_cols[i] == df17_cols[j])
    {
        df17_18_cols<-append(df17_18_cols, df18_cols[i])
        break
    }
}
df18<-df18 %>% select(df17_18_cols)
df17<-df17 %>% select(df17_18_cols)
df17_18<-rbind(df18,df17)
dim(df17_18)
write.csv(df17_18, file="국민건강영양조사(2017~2018).csv", row.names=FALSE)