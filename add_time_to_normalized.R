library(openxlsx)
library(ggplot2)
library(reshape2)
library(dplyr)
options(scipen=999)
olddir = getwd()



parse<-function(x,npar=TRUE,print=TRUE){
  
  s <- strsplit(as.character(x) , ":")
  if (length(s) > 1) {
    min<-sapply(s,"[",1)
    sec<-sapply(s,"[",2)
    min<-as.numeric(min) * 60
    sec<-as.numeric(sec)
    totalMinutes=min + sec
    return (totalMinutes)
  } else {
    return (as.numeric(x))
  }
}

perf.Data <- read.csv("Data/MicrosurgeryPerformance.csv")

normalized <- read.csv("Data/Normalised_Data.csv")

casted <- dcast(normalized,  Session ~ Task)

# Remove last row
perf.Data <- perf.Data[1:15,]

#perf.Data$Cutting.Time.1 <- parse(perf.Data$Cutting.Time.1)
perf.Data$ID <- NULL
perf.Data$Age <- NULL
perf.Data$MS.Year <- NULL
perf.Data$Sex <- NULL
perf.Data$Sutures.1 <- perf.Data$Sutures.2 <- perf.Data$Sutures.3 <- perf.Data$Sutures.4 <- perf.Data$Sutures.5 <- NULL

perf.Data <- perf.Data[,c(1,2,7,8,13,14,19,20,25,26)]

data <- NULL

start = 1
end = start + 1
d <- perf.Data[,c(start:end)]
colnames(d) <- c("CuttingTime","SuturingTime")
d$CuttingTime <- parse(d$CuttingTime)
d$SuturingTime <- parse(d$SuturingTime)
d$session <- "1"
d$ID <- c(1,2,3,4,7,8,10,11,12,13,19,21,22,24,26)
data <- rbind(data,d)



start = 3
end = start + 1
d <- perf.Data[,c(start:end)]
colnames(d) <- c("CuttingTime","SuturingTime")
d$CuttingTime <- parse(d$CuttingTime)
d$SuturingTime <- parse(d$SuturingTime)
d$session <- "2"
d$ID <- c(1,2,3,4,7,8,10,11,12,13,19,21,22,24,26)
data <- rbind(data,d)

start = 5
end = start + 1
d <- perf.Data[,c(start:end)]
colnames(d) <- c("CuttingTime","SuturingTime")
d$session <- "3"
d$ID <- c(1,2,3,4,7,8,10,11,12,13,19,21,22,24,26)
d$CuttingTime <- parse(d$CuttingTime)
d$SuturingTime <- parse(d$SuturingTime)
data <- rbind(data,d)

start = 7
end = start + 1
d <- perf.Data[,c(start:end)]
colnames(d) <- c("CuttingTime","SuturingTime")
d$session <- "4"
d$ID <- c(1,2,3,4,7,8,10,11,12,13,19,21,22,24,26)
d$CuttingTime <- parse(d$CuttingTime)
d$SuturingTime <- parse(d$SuturingTime)
data <- rbind(data,d)

start = 9
end = start + 1
d <- perf.Data[,c(start:end)]
colnames(d) <- c("CuttingTime","SuturingTime")
d$session <- "5"
d$ID <- c(1,2,3,4,7,8,10,11,12,13,19,21,22,24,26)
d$CuttingTime <- parse(d$CuttingTime)
d$SuturingTime <- parse(d$SuturingTime)
data <- rbind(data,d)

#Split by ID
out <- split( data , f = data$ID )

out[[1]]


final <- NULL

for(i in 1:15){
  final <- rbind(final,out[[i]])
}

colnames(final) <- c("Cutting","Suturing","Session","Subject")
final$Session <- as.numeric(final$Session)

final.melt <- melt(final, c(3,4))

# appednd the same data to it because time is same for scorer 2 also
final.melt <- rbind(final.melt,final.melt)

normalized$Time <- final.melt$value

write.csv(normalized, file="Data/Normalised_Data.csv")
