installed.packages("lme4")
library(nlme)
newdata=na.omit(df)
attach(newdata)
Age=newdata$Age
Score1=newdata$Score1
year=newdata$Year
sex=newdata$Sex
session=newdata$Session
subject=newdata$Subject
perspiration=newdata$Normalised_Data
Task=newdata$Task
result=lme(as.numeric(as.character(Score1))~as.numeric(as.character(perspiration))+Age+sex+Task+session,random = ~1 | Subject,data = newdata)
summary(result)
summary(anova(result))