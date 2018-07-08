library(openxlsx)
library(readxl)
library(ggplot2)
library(reshape2)
library(dplyr)
library(nlme)
library(GGally)
library(corrplot)
#library(plot3D)
#library(shiny)
#library(rgl)
library(stringr)
library("ggpubr")
library("Hmisc")
dir=getwd()
dir
data=read.csv("Data/Normalised_Data.csv")
data$Sex[data$Sex==1]="Male"
data$Sex[data$Sex==2]="Female"
Subjects=rep(c("S1","S2","S3","S4","S7","S8","S10","S11","S12","S13","S19","S21","S22","S24","S26"),each=5)
Subjects=rep(Subjects,4)
data$Subjects=Subjects
tai=Subject_Number=c()
for (i in  c(1,2,3,4,5,6,7,8,9, 10,11, 12, 13,19,20,21,22,23,24,25,26)){
  
  if(i<10){
    path=paste("Data/subject0",i,"/","Subject0",i,"_tp.csv",sep="")
  }
  else{
    path=paste("Data/subject",i,"/","Subject",i,"_tp.csv",sep="")
  }
  temp=read.csv(path,header=TRUE)
  v1=as.numeric(str_extract(colnames(temp)[2], "\\-*\\d+\\.*\\d*"))
  tai=append(tai,v1)
  Subject_Number=append(Subject_Number,paste("S",i,sep=""))
  
}
tai_data=data.frame(Subject_Number,tai)
tai_data=tai_data[-c(5,6,9,15,18,20),]
tai=rep(rep(tai_data$tai,each=5),4)
data$Tai=tai


head(data)
#function to plot mean point
give.n <- function(x){
  return(data.frame(y = max(x)+1,
                    label = paste0("n = ",length(x))))
}

#Assumptions:

shapiro.test(data$Scores)
ggqqplot(data$Scores, ylab = "Scores")

new_data=data[,2:13]
new_data$Sessions=rep(c(1:5),60)
drop <- c("Mean_Perspiration")
new_data = (new_data[,!(names(new_data) %in% drop)])
my_num_data <- new_data[, sapply(new_data, is.numeric)]
cor_2 <- rcorr(as.matrix(my_num_data))
cor_2


#Normalised

# # m1=abs(min(na.exclude(data$Mean_Perspiration)))
# col=data$Mean_Perspiration
# col=as.numeric(col)+m1+0.0015
# 
# list1=data$Mean_Perspiration
# #list1=na.omit(df$Mean_Perspiration)
# list1[is.na(list1)] <- 555
# minimumvalue=abs(min(list1))
# 
# for(i in c(1:length(list1))) {
#   list1[i]=list1[i]+minimumvalue+0.0015
#   #appendedlist=append(appendedlist,newlist)
#   print(list1[i])
# }
# 
# for(i in c(1:length(list1))) {
#   if (list1[i] > 500) {
#     list1[i]=NA
#     print(list1[i])
#   }
# }
# print(list1)
# data["Normalised_PP"]<-NA
# data$Normalised_PP=list1
# write.csv(data,"Data/Normalised_Data.csv")


#Performance WRT Sutures
data2=read_excel("Data/MicrosurgeryPerformance.xlsx")
data2=data2[1:15,]
Subject <- rep(rep(unlist(data2$ID),each=5),2)
session=rep(rep(c(1:5),15),2)#rep(c("Session1","Session2","Session3","Session4","Session5"),15)
scorer=rep(c("Scorer1","Scorer2"),each=75)
Scores=append(data$Scores[76:150],data$Scores[226:300])

Sutures=c()
for( i in 1:15){
  Sutures=append(Sutures,c( as.numeric(data2[i,"Sutures 1"]),as.numeric(data2[i,"Sutures 2"]) ,as.numeric(data2[i,"Sutures 3"]) ,as.numeric(data2[i,"Sutures 4"]) ,as.numeric(data2[i,"Sutures 5"])         ))
}
Sutures=rep(Sutures,2)
Age=rep(rep(unlist(data2$Age),each=5),2)
Sex=rep(rep(unlist(data2$Sex),each=5),2)
sdata=data.frame(Subject,Age, Sex,session,Sutures,scorer,Scores)
sdata$Time=rep(data[76:150,"Time"],2)
attach(sdata)
interaction.plot(x.factor     = session,
                 trace.factor = Sex, 
                 response     = Sutures, 
                 col=c("red","green"),  ### Colors for levels of trace var.
                 
                 ### Order by factor order in data
)
for (i in 1:length(Sex)){
  if(as.numeric(Sex[i])==1){
    Sex[i]="Male"
  }
  else if(as.numeric(Sex[i])==2) {
    Sex[i]="Female"
  }
}
p1=ggplot(sdata,aes(session, Sutures, colour=Sex))+ 
  geom_jitter(alpha=0.2) +geom_smooth(method=lm)+xlim(1,5)+theme_light()+theme(plot.title = element_text(hjust = 0.5))+
  labs(x = 'Sessions', y = 'Number of Sutures')+ggtitle("Effect of Sex on Number of Sutures")+theme(panel.grid.major = element_line(colour = 'transparent'))
ggsave(filename="Plot/SuturingVsSex.pdf", plot=p1)

#session=rep(c(1:5),15)#rep(c("Session1","Session2","Session3","Session4","Session5"),15)
#cor(Sutures,session)
#summary(lm(Sutures~Sex+session))

p2=ggplot(sdata,aes(session, Sutures,fill=session)) +theme_light()+ theme(plot.title = element_text(hjust = 0.5)) + 
  #geom_jitter(alpha=0.2) +geom_smooth(method=lm)+xlim(1,5)+
  geom_bar(stat="identity",fill="#56B4E9")+
  theme(panel.grid.major = element_line(colour = 'transparent'))+
  labs(x = 'Sessions', y = 'Number of Sutures')+ggtitle("Performance WRT Sutures of all subjects")
ggsave(filename="Plot/PerformanceWRTSutures_bar.pdf", plot=p2)


p3=ggplot(sdata,aes(session, Sutures,fill=session)) +
  geom_jitter(alpha=0.2) +geom_smooth(method=lm)+xlim(1,5)+theme_light()+theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(hjust = 0.5))+
  #geom_bar(stat="identity",fill="#56B4E9")+
  labs(x = 'Sessions', y = 'Number of Sutures')+ggtitle("Performance WRT number of sutures made")+
  theme(panel.grid.major = element_line(colour = 'transparent'))
ggsave(filename="Plot/PerformanceWRTSutures.pdf", plot=p3)

sdata$Sex=Sex
  

my_num_data1 <- sdata[, sapply(sdata, is.numeric)]
cor_3 <- rcorr(as.matrix(my_num_data1))
cor_3

summary(lm(Sutures~session+Scores+Sex+Age+Time,data=sdata))


#Summary_Based on Scores with all other attributes
model=(lm(formula = Scores~log(Normalised_PP)+Age+Sex+Task+Scorer+Session,data=data))
summary(model)
#Random Effect


rand_data=na.omit(data)
model2=(lme(Scores~Normalised_PP+Age+Year+Sex+Task+Session,random=~1|Subjects,data=rand_data,method="REML"))
anova(model2)



p4 = ggplot(data, aes(x=Scorer, y=Scores,fill=Scorer))  +theme_light()+theme(plot.title = element_text(hjust = 0.5)) +  
  geom_boxplot()+ggtitle("Analysis of Scores based on Scorer") +labs(x="Scorer Number",y="Number of Scores")+stat_summary(fun.y="mean", geom="point", size=1, pch=16, color="red") +
  stat_summary(fun.data = give.n, geom = "text")+theme(panel.grid.major = element_line(colour = 'transparent'))
ggsave(filename="Plot/ScorerVsScore.pdf", plot=p4)


p5 = ggplot(data, aes(x=Sex, y=Scores,fill=Sex))  +theme_light()+theme(plot.title = element_text(hjust = 0.5)) +  
  geom_boxplot()+ggtitle("Analysis of Scores based on Gender") +labs(x="Gender",y="Number of Scores")+stat_summary(fun.y="mean", geom="point", size=1, pch=16, color="red") +
  stat_summary(fun.data = give.n, geom = "text")+theme(panel.grid.major = element_line(colour = 'transparent'))
ggsave(filename="Plot/GenderVsScore.pdf", plot=p5)


p6 = ggplot(data, aes(x=Session, y=Scores,fill=Session))  +theme_light()+ theme(plot.title = element_text(hjust = 0.5)) +  
  geom_boxplot()+ggtitle("Analysis of Scores based on Scorer") +labs(x="Session Number",y="Number of Scores")+stat_summary(fun.y="mean", geom="point", size=1, pch=16, color="red") +
  stat_summary(fun.data = give.n, geom = "text")+theme(panel.grid.major = element_line(colour = 'transparent'))
ggsave(filename="Plot/SessionVsScore.pdf", plot=p6)


p7 = ggplot(data, aes(x=Task, y=Scores,fill=Task))  + theme_light()+theme(plot.title = element_text(hjust = 0.5)) +  
  geom_boxplot()+ggtitle("Analysis of Scores based on Task") +labs(x="Task",y="Number of Scores")+stat_summary(fun.y="mean", geom="point", size=1, pch=16, color="red") +
  stat_summary(fun.data = give.n, geom = "text")+theme(panel.grid.major = element_line(colour = 'transparent'))
ggsave(filename="Plot/TaskVsScore.pdf", plot=p7)

#Summary_Based on Time with all other attributes
summary(lm(formula = Time~log(Normalised_PP)+Age+Sex+Task+Session,data=data))

p8 = ggplot(data, aes(x=Scorer, y=Time,fill=Scorer))  + theme_light()+theme(plot.title = element_text(hjust = 0.5)) +  
  geom_boxplot()+ggtitle("Analysis of Time based on Scorer") +labs(x="Scorer Number",y="Time in seconds")+stat_summary(fun.y="mean", geom="point", size=1, pch=16, color="red") +
  stat_summary(fun.data = give.n, geom = "text",vjust = 0)+ylim(0,1300)+theme(panel.grid.major = element_line(colour = 'transparent'))
ggsave(filename="Plot/ScorerVsTime.pdf", plot=p8)


p9 = ggplot(data, aes(x=Sex, y=Time,fill=Sex)) + theme_light()+theme(plot.title = element_text(hjust = 0.5)) +  
  geom_boxplot()+ggtitle("Analysis of Time based on Gender") +labs(x="Gender",y="Time in seconds")+stat_summary(fun.y="mean", geom="point", size=1, pch=16, color="red") +
  stat_summary(fun.data = give.n, geom = "text",vjust = 0)+theme(panel.grid.major = element_line(colour = 'transparent'))
ggsave(filename="Plot/GenderVsTime.pdf", plot=p9)


p10 = ggplot(data, aes(x=Session, y=Time,fill=Session))  +theme_light()+ theme(plot.title = element_text(hjust = 0.5)) +  
  geom_boxplot()+ggtitle("Analysis of Time based on Scorer") +labs(x="Session Number",y="Time in seconds")+stat_summary(fun.y="mean", geom="point", size=1, pch=16, color="red") +
  stat_summary(fun.data = give.n, geom = "text",vjust = 0)+theme(panel.grid.major = element_line(colour = 'transparent'))
ggsave(filename="Plot/SessionVsTime.pdf", plot=p10)


p11 = ggplot(data, aes(x=Task,y=Time,fill=Task))  +theme_light()+ theme(plot.title = element_text(hjust = 0.5)) +  
  geom_boxplot()+ggtitle("Analysis of Time based on Task") +labs(x="Task",y="Time in seconds")+stat_summary(fun.y="mean", geom="point", size=1, pch=16, color="red") +
  stat_summary(fun.data = give.n, geom = "text",vjust = 0)+theme(panel.grid.major = element_line(colour = 'transparent'))
ggsave(filename="Plot/TaskVsTime.pdf", plot=p11)





#Analysis based on Scorer
Cutting=read.csv("Data/Cutting_Data.csv")
p12=ggplot(Cutting, aes(x=Scorer, y=Scores,fill=Scorer)) +theme_light()+ theme(plot.title = element_text(hjust = 0.5)) +  
  geom_boxplot()+ggtitle("Analysis of Scores based on Scorer") +labs(x="Scorer Number",y="Scores")+stat_summary(fun.y="mean", geom="point", size=1, pch=16, color="red") +
  stat_summary(fun.data = give.n, geom = "text")+theme(panel.grid.major = element_line(colour = 'transparent'))
ggsave(filename="Plot/Cutting_ScorerVsScore.pdf", plot=p12)

CScorer1=as.numeric(Cutting[1:75,"Scores"])
CScorer2=as.numeric(Cutting[76:150,"Scores"])
#Assumption
shapiro.test(CScorer1)
shapiro.test(CScorer2)

#Wilcox Test
wilcox.test(CScorer2,CScorer1, paired=TRUE, alternative ="two.sided")

Suturing=read.csv("Data/Suturing_Data.csv")
p13=ggplot(Suturing, aes(x=Scorer, y=Scores,fill=Scorer)) +theme_light()+theme(plot.title = element_text(hjust = 0.5)) +
  geom_boxplot()+ggtitle("Analysis of Scores based on Scorer") +labs(x="Scorer Number",y="Scores")+stat_summary(fun.y="mean", geom="point", size=1, pch=16, color="red") +
  stat_summary(fun.data = give.n, geom = "text")+theme(panel.grid.major = element_line(colour = 'transparent'))
ggsave(filename="Plot/Suturing_ScorerVsScore.pdf", plot=p13)


SScorer1=as.numeric(Suturing[1:75,"Scores"])
SScorer2=as.numeric(Suturing[76:150,"Scores"])
shapiro.test(SScorer1)
shapiro.test(SScorer2)
#Wilcox Test
wilcox.test(SScorer2,SScorer1, paired=TRUE, alternative ="two.sided")

#Assumption

shapiro.test(Cutting$Scores)
shapiro.test(Suturing$Scores)

#Wilcox Test:
wilcox.test(Cutting$Scores,Suturing$Scores,paired=TRUE, alternative ="two.sided")


p14=ggplot(sdata,aes(Sutures,Time))+theme_light()+ theme(plot.title = element_text(hjust = 0.5))+geom_point()+geom_smooth()+ylim(1150,1210)+ggtitle("Performance Analysis of Suturing") +labs(y="Time in Seconds",x="Number of Sutures made")+theme(panel.grid.major = element_line(colour = 'transparent'))
p14 <- p14 + facet_grid(session~.)

ggsave(filename="Plot/SuturingVsTime.pdf", plot=p14) 






#Correlation : GGPairs
cdata=data.frame(Cutting$Age,Cutting$Sex,Cutting$Session,Cutting$Scorer,Cutting$Scores)
sdata=data.frame(Suturing$Age,Suturing$Sex,Suturing$Session,Suturing$Scorer,Suturing$Scores)
colnames(cdata)=colnames(sdata)=c("Age","Sex","Session","Scorer","Scores")

p15=ggpairs(cdata,columns=2:5 ,aes(col = Scorer, alpha=0.3))+theme_light()+theme(panel.grid.major = element_line(colour = 'transparent'))
ggsave(filename="Plot/Cutting_GGpairs.pdf", plot=p15) 


p16=ggpairs(sdata,columns=2:5 , aes(col = Scorer, alpha=0.2))+theme_light()+theme(panel.grid.major = element_line(colour = 'transparent'))
ggsave(filename="Plot/Suturing_GGpairs.pdf", plot=p16) 


p17=ggpairs(data, columns=5:8,aes(col = Task, alpha=0.2))+theme_light()+theme(panel.grid.major = element_line(colour = 'transparent'))
ggsave(filename="Plot/Data_GGpairs.pdf", plot=p17) 







#Subjects Vs Mean pp

p18=ggplot(data, aes(Subjects, Normalised_PP,fill=Task)) + 
  geom_bar(stat="identity", position = "dodge") + theme_light()+theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_brewer(palette = "Set1")+ggtitle("Analysis of Perspiration based on Subject") +labs(x="Subject Number",y="Normalised Mean Perspiration")++stat_summary(fun.y="mean", geom="point", size=1, pch=16, color="red") +
  stat_summary(fun.data = give.n, geom = "text")+theme(panel.grid.major = element_line(colour = 'transparent'))
ggsave(filename="Plot/SubjectVsPP.pdf", plot=p18)










#Tai Scores Analysis

p19=ggplot(data,aes(Tai,Scores,fill=Task)) + 
  geom_bar(stat="identity", position = "dodge") +theme_light()+theme(plot.title=element_text(hjust = 0.5))+ ggtitle("Performance Analysis of Tai")+labs(x="Tai Score",y="Scores")++stat_summary(fun.y="mean", geom="point", size=1, pch=16, color="red") +
  stat_summary(fun.data = give.n, geom = "text")+theme(panel.grid.major = element_line(colour = 'transparent'))
  
ggsave(filename="Plot/TaiVsScores.pdf",plot=p19)


d1=data
d2=d1[d1$Tai>40,]
d3=d1[d1$Tai<=40,]
Score_below=d3$Scores
Score_Above=d2$Scores
t1=rep("Less than 40",160)
t2=rep("Greater than 40",140)
TAI=append(t1,t2)
Score=append(Score_below[1:160],Score_Above)
nd1=data.frame(TAI,Score)



p20 = ggplot(nd1, aes(x=TAI,y=Score,fill=TAI))  + theme_light()+theme(plot.title = element_text(hjust = 0.5)) +  
  geom_boxplot()+ggtitle("Analysis of Tai Vs Score") +labs(x="Tai",y="Score")+stat_summary(fun.y="mean", geom="point", size=1, pch=16, color="red") +
  stat_summary(fun.data = give.n, geom = "text")+theme(panel.grid.major = element_line(colour = 'transparent'))
ggsave(filename="Plot/TaiVsScores_Box.pdf", plot=p20)

#Assumption:
shapiro.test(Score_Above)
shapiro.test(Score_below)

wilcox.test(Score_below,Score_Above,alternative = "greater")

