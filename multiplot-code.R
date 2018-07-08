library(openxlsx)
library(ggplot2)
library(reshape2)
library(dplyr)
library(gridExtra)
library(grid)
options(scipen=999)
olddir = getwd()
# curdir = "/home/yashwanth/Downloads/spring 18/stats/Methodist microsurgery with output/"
# setwd(curdir)

# subjects

v <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,19,20,21,22,23,24,25,26)

# Code to extract only legend
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# Code for Multiple Plots in Single page obtained from R Tutorials online 
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# Code to Save Graph
savePlot <- function(plot, name) {
  pdf(paste(name))
  print(plot)
  dev.off()
}

# Code to plot the graphs in Question 3
plotGraphs <- function(cutNASA, n){
  p <- ggplot(cutNASA, aes(Session,as.numeric(`Mental Demand`), fill = Session)) + geom_bar(stat = "identity") + xlab("Sessions") +
    ylab("Value") + theme(plot.title = element_text(hjust = 0.5))+ ggtitle(paste(n,"Mental Demand"))
  savePlot(p,paste(n,"Mental Demand.png") )
  
  p <-ggplot(cutNASA, aes(Session,as.numeric(`Physical Demand`), fill = Session)) + geom_bar(stat = "identity") + xlab("Sessions") +
    ylab("Value") + theme(plot.title = element_text(hjust = 0.5))+ ggtitle(paste(n,"Physical Demand"))
  savePlot(p,paste(n,"Physical Demand.png"))
  
  p <-ggplot(cutNASA, aes(Session,as.numeric(`Temporal Demand`), fill = Session)) + geom_bar(stat = "identity") + xlab("Sessions") +
    ylab("Value") + theme(plot.title = element_text(hjust = 0.5))+ ggtitle(paste(n,"Temporal Demand"))
  savePlot(p,paste(n,"Temporal Demand.png"))
  
  p <-ggplot(cutNASA, aes(Session,as.numeric(`Performance`), fill = Session)) + geom_bar(stat = "identity") + xlab("Sessions") +
    ylab("Value") + theme(plot.title = element_text(hjust = 0.5))+ ggtitle(paste(n,"Performance"))
  savePlot(p,paste(n,"Performance.png"))
  
  p <-ggplot(cutNASA, aes(Session,as.numeric(`Effort`), fill = Session)) + geom_bar(stat = "identity") + xlab("Sessions") +
    ylab("Value") + theme(plot.title = element_text(hjust = 0.5))+ ggtitle(paste(n,"Effort"))
  savePlot(p,paste(n,"Effort.png"))
  
  p <-ggplot(cutNASA, aes(Session,as.numeric(`Mental Demand`), fill = Session)) + geom_bar(stat = "identity") + xlab("Sessions") +
    ylab("Value") + theme(plot.title = element_text(hjust = 0.5))+ ggtitle(paste(n,"Frustration"))
  savePlot(p,paste(n,"Frustration.png"))
}

# Code to downsample the signals
downsampling<-function(data){
  smooth_signal_val<-data.frame(Frames.=integer(),Time=integer(),Perspiration=integer())
  pp<-data
  min<-floor(min(pp$Time))
  max<-floor(max(pp$Time))
  for(i in min:max){
    pp_mod<-pp[which(pp$Time >=i & pp$Time <(i+1)),]
    rows=nrow(pp_mod)
    avg_per=sum(pp_mod$Perspiration)/rows
    smooth_signal_val<-rbind(smooth_signal_val,c(1,i,avg_per))
  }
  colnames(smooth_signal_val)<-c("Frames.","Time","Perspiration")
  return(smooth_signal_val)
  
}

# Question 1
Biographic.Data <- function(){
  # Biographic Data: Draw the barplot for gender distribution; histogram for age distribution
  
  #read the data file
  age.Gender.Data <- read.xlsx("MicrosurgeryPerformance.xlsx")
  #remove last row in the data frame
  age.Gender.Data <- age.Gender.Data[1:15,]
  # transform 
  age.Gender.Data <- transform(age.Gender.Data, Sextype= ifelse(Sex==1, "Male", "Female"))
  # plot gender distribution
  
  gender <- ggplot(age.Gender.Data, aes(Sextype, fill=Sextype)) + geom_bar() + xlab("Gender") + ylab("No of Participants") + scale_y_continuous(breaks=c(0,2,4,6,8,10))+
    theme_light() + theme(plot.title = element_text(hjust = 0.5), legend.position="none")+ ggtitle("Barplot for gender distribution")
  # Save the plot 
  savePlot(gender,"1_gender.pdf")
  
  # plot age distribution
  age <- ggplot(age.Gender.Data, aes(Age)) + geom_histogram(bins = 5) + ylab("Frequency") + xlab("Age [Years]")+theme_light() +
    theme(plot.title = element_text(hjust = 0.5))+ ggtitle("Histogram for Age distribution")
  # save the plot 
  savePlot(age, "1_age.pdf")
}
Biographic.Data()

# Question 2
# Trait Psychometric Data: Draw the histogram for TAI scores. Please consider only the total TAI score, 
# which can be found at the *tp.csv file residing at the root of each subject file system.
scores <- read.csv("tai_scores.txt", header = FALSE)

hist(scores$V2,xlim=c(20,80),col='cadetblue', xlab="Tai scores",main="Histogram of Tai scores")
# Question 3
# State Psychometric Data: For each subject draw the barplots for all the NASA-TLX subscales per task. 
# This will give two figures per subject per subscale, one for suturing and one for cutting, where the 
# evolution of the scores from the initial to the final session will be evident. There should be a downward trend, 
# reflecting increased facility with the tasks
State.Pyschometric.Data <- function(){
  plots = NULL
  group.colors <- c("1" ="#8cb369","2"="#f4e285","3"="#f4a259","4"="#5b8e7d","5"="#bc4b51")
  for(i in c(1,2,3,4,5,6,7,8,9, 10, 11, 12, 13,19, 20, 21,22,23,24,25,26)){
    ttl = i
    path = paste("subject", formatC(i, width=2, flag="0"), sep="")  
    print(path)
    cuttingNasa = list.files(path = path, full.names = TRUE, recursive = TRUE, pattern = "^(.*)(Cutting)(.*)(NASA)(.*?)$")
    suturingNasa = list.files(path = path, full.names = TRUE, recursive = TRUE, pattern = "^(.*)(Suturing)(.*)(NASA)(.*?)$")
    
    Response = c("Mental Demand","Physical Demand", "Temporal Demand", "Performance" ,"Effort" ,"Frustration")
    cutNASA = as.data.frame(Response)
    for(i in cuttingNasa){
      subject = substr(i,27,28)
      temp <- read.csv(i, stringsAsFactors = FALSE)
      colnames(temp) <- c("Response", substr(i,18,18))
      #temp <- rbind(temp, c("subject", subject))
      cutNASA <- dplyr::left_join(cutNASA, temp, by="Response")
    }
    
    sutNASA = as.data.frame(Response)
    for(i in suturingNasa){
      subject = substr(i,27,28)
      temp <- read.csv(i, stringsAsFactors = FALSE)
      colnames(temp) <- c("Response", substr(i,38,38))
      #temp <- rbind(temp, c("subject", subject))
      sutNASA <- dplyr::left_join(sutNASA, temp, by="Response")
    }
    
    #added new by yash
    cutNASA <- melt(cutNASA, id.vars='Response')
    cutNASA$action <- "Cutting"
      
    sutNASA <- melt(sutNASA, id.vars='Response')
    sutNASA$action <- "Suturing"
    
    t <- rbind(cutNASA, sutNASA)
    ttl <- paste("Subject ",ttl)
    q <- ggplot(t, aes(Response, as.integer(value), fill = factor(variable))) + ylim(0,20) + theme_light() +
      geom_bar(position = "dodge", stat="identity")  + ylab("Scores") + xlab("Response") + theme(legend.position = "none") +
      theme(axis.title.y = element_text(size=6), axis.title.x = element_text(size=6), axis.text.y = element_text(size=5),axis.text.x = element_text(angle=25, hjust=1, size = 5),plot.title = element_text(hjust = 0.5, size=4)) + scale_fill_manual(name="Sessions",values=group.colors) + ggtitle(paste(ttl, "State Psychometric Data")) 
    q <- q + facet_grid(action ~ .)
    q
    
    plots[[i]] <- q
    
    #pdf(paste(path,"State_Psychometric_Data_of_Cutting_and_Suturing.pdf",sep="_"))
    #print(q)
    #dev.off()
    
  }
  
  #grid.draw(g_legend(q+ theme(legend.position = "bottom")))
  #plots[[1]] <- plots[[1]] + theme(legend.position = "right")
  plots[[2]] <- plots[[2]] + ylab("Score")
  #plots[[7]] <- plots[[7]] + ggtitle("State Psychometric Data of Cutting and Suturing") 
  plots[[9]] <- plots[[9]] + xlab("Response") 
  multiplot(plots[[1]],plots[[2]], plots[[3]],plots[[4]],plots[[5]],plots[[6]],plots[[7]],plots[[8]],plots[[9]],plots[[10]],plots[[11]],plots[[12]],plots[[13]],plots[[14]],plots[[15]],plots[[16]],plots[[17]],plots[[18]],plots[[19]],plots[[20]],plots[[21]], cols = 7)
  multiplot(plots[[1]],plots[[2]], plots[[3]],plots[[4]],plots[[5]],plots[[6]],plots[[7]],plots[[8]],plots[[9]],plots[[10]],plots[[11]],plots[[12]], cols = 4)
  multiplot(plots[[13]],plots[[14]],plots[[15]],plots[[16]],plots[[17]],plots[[18]],plots[[19]],plots[[20]],plots[[21]], cols = 4)
  
}
State.Pyschometric.Data()

# Question 4
# Perinasal Perspiration (Stress) Signal Data: For each session of each subject draw the stress signals, 
# using black for baseline, green for cutting, and red for suturing. Generally speaking, the baseline signal 
# should be at a lower level than the other two. In total, you will draw five figures for each subject or 
# whatever the number of his/her sessions is.

Perinasal.Perspiration <- function(){
  plots <- NULL
  group.colors <- c(Baseline="#000000",Cutting="#00FF00",Suturing="#FF0000")
  for(i in v){
    path = paste("subject", formatC(i, width=2, flag="0"), sep="")  
    print(path)
    
    #path = paste(path, path, sep="/")
    sessions = list.dirs(path)
    
    
    data <- NULL
    for(p in sessions[-1]){
      session = substr(p,18,18)
      baseline = list.files(path = p, full.names = TRUE, recursive = TRUE, pattern = "^(.*)(Baseline)[[:digit:]]{1}\\.csv$")
      cutting = list.files(path = p, full.names = TRUE, recursive = TRUE, pattern = "^(.*)(Cutting)[[:digit:]]{1}\\.csv$")
      suturing = list.files(path = p, full.names = TRUE, recursive = TRUE, pattern = "^(.*)(Suturing)[[:digit:]]{1}\\.csv$")
      if(length(baseline) > 0){
        baseline[1]
        b <- read.csv(baseline[1])
        b <- downsampling(b)
        b$Frames. <- NULL
        b$task <- paste("Baseline")
        b$session <- paste("Session",session)
        data <- rbind(data,b)
      } 
      if(length(cutting) > 0){
        cutting[1]
        c <- read.csv(cutting[1])
        c <- downsampling(c)
        c$Frames. <- NULL
        c$task <- paste("Cutting")
        c$session <- paste("Session",session)
        data <- rbind(data,c)
      } 
      if(length(suturing) > 0){
        suturing[1]
        s <- read.csv(suturing[1])
        s <- downsampling(s)
        s$Frames. <- NULL
        s$task <- paste("Suturing")
        s$session <- paste("Session",session)
        data <- rbind(data, s)
      } 
      
      
      
      # if (length(b) > 0){
      #   p1 <- geom_line(data=b,aes(Time, Perspiration))
      # } else {
      #   p1 <- NULL
      # }
      # if (length(c) > 0){
      #   p2 <- geom_line(data=c,aes(Time, Perspiration), colour="green")
      # } else {
      #   p2 <- NULL
      # }
      # if (length(s) > 0){
      #   p3 <- geom_line(data=s,aes(Time, Perspiration,colour="red")) 
      # } else {
      #   p3 <- NULL
      # }
      
      
      
      
      
      # p5 <- ggplot() + xlab("") +
      #   theme(plot.title = element_text(hjust = 0.5))+ ggtitle(paste("Perinasal Perspiration", session, sep = "Session ")) + xlab("Time") +
      #   scale_fill_manual(values=c("black", "red", "green"), 
      #                     name="Experimental\nCondition",
      #                     labels=c("Baseline", "Cutting", "Suturing")) + theme(legend.title=element_blank()) + 
      # png(paste(n,"Perinasal Perspiration.png")) 
      # print(p5 + p1 + p2 + p3)
      # #multiplot(p1,p2,p3, cols=1)
      # dev.off()
      # 
      # b <- NULL
      # c <- NULL
      # s <- NULL
      
    }
    
    
    title<-paste("Perinasal Perspiration (Stress) Signal for Subject ",1)
    p <- ggplot(data, aes(x=Time, y= Perspiration, col=factor(task))) + geom_line()
    p <- p +  xlab("Time[S]") +  theme_light() +
      ylab(expression(paste("Perinasal Perspiration"))) +
      geom_line() + theme(legend.position = "none")  +  ggtitle(title) + 
      scale_colour_manual(values=group.colors) + theme(axis.title.y = element_text(size=6), axis.title.x = element_text(size=6), axis.text.y = element_text(size=5),axis.text.x = element_text(angle=25, hjust=1, size = 5),plot.title = element_text(hjust = 0.5, size=4))  +
      guides(color=guide_legend(title="Task")) 
    p <- p + facet_grid(session ~ . , scales = "free")
    #pdf(paste(substr(path,8,9),"Perinasal_Perspiration.pdf",sep="_")) 
    #print(p)
    #dev.off()
    print(i)
    plots[[i]] <- p
    
  }
  
  plots[[2]] <- plots[[2]] + ylab("Perspiration")
  plots[[9]] <- plots[[9]] + xlab("Time") 
  multiplot(plots[[1]],plots[[2]], plots[[3]],plots[[4]],plots[[5]],plots[[6]],plots[[7]],plots[[8]],plots[[9]],plots[[10]],plots[[11]],plots[[12]], cols = 4)
  #multiplot(plots[[1]],plots[[2]], plots[[3]],plots[[4]],plots[[5]],plots[[6]],plots[[7]],plots[[8]],plots[[9]],plots[[10]],plots[[11]],plots[[12]],plots[[13]],plots[[14]],plots[[15]],plots[[16]],plots[[17]],plots[[18]],plots[[19]],plots[[20]],plots[[21]], cols = 5)
  multiplot(plots[[13]],plots[[19]],plots[[20]],plots[[21]],plots[[22]],plots[[23]],plots[[24]],plots[[25]],plots[[26]], cols = 4)
  
  
}
Perinasal.Perspiration()

# Question 5
# Performance Data: Draw the time barplots for each subject, using different colors for each task (cutting vs. suturing).
# You should observe a downward trend. Draw also the accuracy barplots for each subject, using different colors for 
# each task (cutting vs. suturing). You should observe an ascending trend.

### Read the data

parse<-function(x,npar=TRUE,print=TRUE){

  s <- strsplit(as.character(x) , ":")
  if (length(s) > 1) {
    min<-sapply(s,"[",1)
    sec<-sapply(s,"[",2)
    min<-as.numeric(min)
    sec<-as.numeric(sec)
    totalMinutes=min * 60 + (sec)
    return (totalMinutes)
  } else {
    return (as.numeric(x))
  }
}

perf.Data <- read.csv("MicrosurgeryPerformance.csv")
# Remove last row
perf.Data <- perf.Data[1:15,]

#perf.Data$Cutting.Time.1 <- parse(perf.Data$Cutting.Time.1)
perf.Data$ID <- NULL
perf.Data$Age <- NULL
perf.Data$MS.Year <- NULL
perf.Data$Sex <- NULL
perf.Data$Sutures.1 <- perf.Data$Sutures.2 <- perf.Data$Sutures.3 <- perf.Data$Sutures.4 <- perf.Data$Sutures.5 <- NULL

data <- NULL

start = 1
end = start + 5
d <- perf.Data[,c(start:end)]
colnames(d) <- c("CuttingTime","SuturingTime","Score1Cut","Score1Sut","Score2Cut","Score2Sut")
d$CuttingTime <- parse(d$CuttingTime)
d$SuturingTime <- parse(d$SuturingTime)
d$Score1Cut <- as.numeric(d$Score1Cut)
d$Score2Cut <- as.numeric(d$Score2Cut)
d$Score1Sut <- as.numeric(d$Score1Sut)
d$Score2Sut <- as.numeric(d$Score2Sut)
d$session <- "1"
d$ID <- c(1,2,3,4,7,8,10,11,12,13,19,21,22,24,26)
data <- rbind(data,d)



start = 7
end = start + 5
d <- perf.Data[,c(start:end)]
colnames(d) <- c("CuttingTime","SuturingTime","Score1Cut","Score1Sut","Score2Cut","Score2Sut")
d$CuttingTime <- parse(d$CuttingTime)
d$SuturingTime <- parse(d$SuturingTime)
d$Score1Cut <- as.numeric(d$Score1Cut)
d$Score2Cut <- as.numeric(d$Score2Cut)
d$Score1Sut <- as.numeric(d$Score1Sut)
d$Score2Sut <- as.numeric(d$Score2Sut)
d$session <- "2"
d$ID <- c(1,2,3,4,7,8,10,11,12,13,19,21,22,24,26)
data <- rbind(data,d)

start = 13
end = start + 5
d <- perf.Data[,c(start:end)]
colnames(d) <- c("CuttingTime","SuturingTime","Score1Cut","Score1Sut","Score2Cut","Score2Sut")
d$session <- "3"
d$ID <- c(1,2,3,4,7,8,10,11,12,13,19,21,22,24,26)
d$CuttingTime <- parse(d$CuttingTime)
d$SuturingTime <- parse(d$SuturingTime)
d$Score1Cut <- as.numeric(d$Score1Cut)
d$Score2Cut <- as.numeric(d$Score2Cut)
d$Score1Sut <- as.numeric(d$Score1Sut)
d$Score2Sut <- as.numeric(d$Score2Sut)
data <- rbind(data,d)

start = 19
end = start + 5
d <- perf.Data[,c(start:end)]
colnames(d) <- c("CuttingTime","SuturingTime","Score1Cut","Score1Sut","Score2Cut","Score2Sut")
d$session <- "4"
d$ID <- c(1,2,3,4,7,8,10,11,12,13,19,21,22,24,26)
d$CuttingTime <- parse(d$CuttingTime)
d$SuturingTime <- parse(d$SuturingTime)
d$Score1Cut <- as.numeric(d$Score1Cut)
d$Score2Cut <- as.numeric(d$Score2Cut)
d$Score1Sut <- as.numeric(d$Score1Sut)
d$Score2Sut <- as.numeric(d$Score2Sut)
data <- rbind(data,d)

start = 25
end = start + 5
d <- perf.Data[,c(start:end)]
colnames(d) <- c("CuttingTime","SuturingTime","Score1Cut","Score1Sut","Score2Cut","Score2Sut")
d$session <- "5"
d$ID <- c(1,2,3,4,7,8,10,11,12,13,19,21,22,24,26)
d$CuttingTime <- parse(d$CuttingTime)
d$SuturingTime <- parse(d$SuturingTime)
d$Score1Cut <- as.numeric(d$Score1Cut)
d$Score2Cut <- as.numeric(d$Score2Cut)
d$Score1Sut <- as.numeric(d$Score1Sut)
d$Score2Sut <- as.numeric(d$Score2Sut)
data <- rbind(data,d)

#Split by ID
out <- split( data , f = data$ID )

out[[1]]

plots <- out

for (i in 1:15) {
  t <- out[[i]]
  title <- paste("Time Barplot for Subject ", i)
  q <- melt(t, id=c("ID","session","Score2Sut","Score2Cut","Score1Sut","Score1Cut"))
  p <- ggplot(q, aes(session,value, goup=variable , fill=variable)) +  geom_bar(stat="identity",position = "dodge")
  p <- p +  xlab("Sessions") + theme_light() +
    ylab("Time[secs]") + theme(legend.position = "none") + ggtitle(title) +
    theme(axis.title.y = element_text(size=6), axis.title.x = element_text(size=6), axis.text.y = element_text(size=5),axis.text.x = element_text(angle=25, hjust=1, size = 5),plot.title = element_text(hjust = 0.5, size=4))  + 
    scale_fill_discrete(name = "Task", labels = c("Cutting", "Suturing")) 
  #pdf(paste(i,"Time_barplot.pdf",sep = "_")) 
  #print(p)
  #dev.off()
  plots[[i]] <- p
}
multiplot(plots[[1]],plots[[2]], plots[[3]],plots[[4]],plots[[5]],plots[[6]],plots[[7]],plots[[8]],plots[[9]], cols = 3)
multiplot(plots[[10]],plots[[11]],plots[[12]],plots[[13]],plots[[14]],plots[[15]], cols = 3)

for (i in 1:15) {
  t <- out[[i]]
  t$CuttingTime <- t$SuturingTime <- NULL
  title <- paste("Score Barplot for Subject ", i)
  q <- melt(t, id=c("ID","session","Score2Sut","Score2Cut"))
  r <- melt(t, id=c("ID","session","Score1Sut","Score1Cut"))
  colnames(q) <- colnames(r) <- c("ID","session","Sut","Cut","variable","value")
  q$scorer <- "Scorer 2"
  r$scorer <- "Scorer 1"
  r <- rbind(r,q)
  r$variable <- substr(r$variable,7,9)
  u <- ggplot(r, aes(session,value, goup=variable , fill=variable)) +  geom_bar(stat="identity",position = "dodge") + xlab("Sessions") + theme_light() +
    ylab("Score") + theme(legend.position = "none") + ggtitle(title) +
    theme(axis.title.y = element_text(size=6), axis.title.x = element_text(size=6), axis.text.y = element_text(size=5),axis.text.x = element_text(angle=25, hjust=1, size = 5),plot.title = element_text(hjust = 0.5, size=4))  + 
    scale_fill_discrete(name = "Task", labels = c("Cutting", "Suturing"))
  u <- u + facet_grid(scorer~.)
  
  #pdf(paste(i,"Score_barplot.pdf",sep="_")) 
  
  #multiplot(p, u, cols = 1)
  #print(u)
  #dev.off()
  plots[[i]] <- u
}

multiplot(plots[[1]],plots[[2]], plots[[3]],plots[[4]],plots[[5]],plots[[6]],plots[[7]],plots[[8]],plots[[9]],plots[[10]],plots[[11]],plots[[12]],plots[[13]],plots[[14]],plots[[15]], cols = 5)
multiplot(plots[[1]],plots[[2]], plots[[3]],plots[[4]],plots[[5]],plots[[6]],plots[[7]],plots[[8]],plots[[9]], cols = 3)
multiplot(plots[[10]],plots[[11]],plots[[12]],plots[[13]],plots[[14]],plots[[15]], cols = 3)
