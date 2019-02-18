#!/bin/bash

# https://archive.ics.uci.edu/ml/datasets/Absenteeism+at+work
# EDA CHECKLIST
# 1. formulate you question
# What is the most convincing reason for absenses 
# (reason + support issues) based on dataset? 
# Compare to articles in US
    
# 2. read your data
WorkAbs<-read.csv("Absenteeism_at_work.csv",
    sep=";",header=T)

# 3. check you packaging
# dim(WorkAbs)
# 740 Observations (rows)
# 21 Variables (columns)

# 4. run str()
str(WorkAbs)
# 'data.frame':	740 obs. of  21 variables:

# $ ID                             : int  11 36 3 7 11 3 10 20 14 1 ...
# $ Reason.for.absence             : int  26 0 23 7 23 23 22 23 19 22 ...

# time profile --------------------------------------------------
# $ Month.of.absence               : int  7 7 7 7 7 7 7 7 7 7 ...
# $ Day.of.the.week                : int  3 3 4 5 5 6 6 6 2 2 ...
# $ Seasons                        : int  1 1 1 1 1 1 1 1 1 1 ...
# $ Absenteeism.time.in.hours      : int  4 0 2 4 2 2 8 4 40 8 ...

# distance profile ----------------------------------------------
# $ Transportation.expense         : int  289 118 179 279 289 179 361 260 155 235 ...
# $ Distance.from.Residence.to.Work: int  36 13 51 5 36 51 52 50 12 11 ...

# Professional Profile -------------------------------------------
# $ Service.time                   : int  13 18 18 14 13 18 3 11 14 14 ...
# $ Work.load.Average.day          : num  240 240 240 240 240 ...
# $ Hit.target                     : int  97 97 97 97 97 97 97 97 97 97 ...
# $ Disciplinary.failure           : int  0 1 0 0 0 0 0 0 0 0 ...

# Lifestyle profile ----------------------------------------------
# $ Education                      : int  1 1 1 1 1 1 1 1 1 3 ...
# $ Son                            : int  2 1 0 2 2 0 1 4 2 1 ...
# $ Social.drinker                 : int  1 1 1 1 1 1 1 1 1 0 ...
# $ Social.smoker                  : int  0 0 0 1 0 0 0 0 0 0 ...
# $ Pet                            : int  1 0 0 0 1 0 4 0 0 1 ...

# Physical Profile ----------------------------------------------
# $ Weight                         : int  90 98 89 68 90 89 80 65 95 88 ...
# $ Height                         : int  172 178 170 168 172 170 172 168 196 172 ...
# $ Body.mass.index                : int  30 31 31 24 30 31 27 23 25 29 ...
# $ Age                            : int  33 50 38 39 33 38 28 36 34 37 ...

# 5. run a summary of dataset
summary(WorkAbs)

# 6. check your "n"s
sum(is.na(WorkAbs))
# [1] 0

# 7. validate with at least one external data source
# data set is correct

# 8. try the easy solution first
WorkAbs$Reason.for.absence<-as.factor(WorkAbs$Reason.for.absence)
getmode<-function(v){
   uniqv<-unique(v)
   uniqv[which.max(tabulate(match(v,uniqv)))]
}
getmode(WorkAbs$Reason.for.absence)
# [1] 23
# medical consultation
AbsReasons1<-table(WorkAbs$Reason.for.absence)
AbsReasons2<-as.data.frame(AbsReasons1)
AbsReasons2Var1<-as.character(AbsReasons2$Var1)
# viz1
png("Plot1",width=480,height=480)
m<-rbind(c(1,1,1),c(2,3,4))
layout(m)
par(mar=c(5,5,5,2),len=1)
boxplot(WorkAbs$Absenteeism.time.in.hours~WorkAbs$Reason.for.absence,
        main="Work Hours Missed by Category",
        xlab="Reasons for Absence by Category",
        ylab="Work Hours Missed",cex.names=0.8)
title(list("Initial Discovery of The Reason for Abseentism by 4 Plots",cex=1.5,
           col="red",font = 6),line=-1.5,outer=T)
plot(WorkAbs$Absenteeism.time.in.hours,WorkAbs$Distance.from.Residence.to.Work,
     main="Absenteesim Related to \nDistance From Work",
     xlab="Abseentism in Hours",
     ylab="Distance From Home to Work")
barplot(AbsReasons2$Freq,
    main="Top 10% Absense Reasons \nby Frequency",
    xlab="Frequency",
    ylab="Reasons for Absence by Category",
    names.arg=AbsReasons2$Var1,
    horiz=T)
    abline(v=quantile(AbsReasons2$Freq,0.9),
        col="red",lty=3,lwd=3)
boxplot(AbsReasons2$Freq,
        main="Absence Reasons \nOutlier Detection",
        xlab="Frequency Distribution of \nAbsence Reasons")
dev.off()
# outlier detection for freq in hours resulted in poor sample of population
# frequency of Reasons more valid than freq of hours per category
# determined 28, 27, 23 & almost 13
## categories without (CID) patient follow-up
# 23 [1-149]: medical consultation
# 28 [2-112]: dental consultation
# 27 [3-069]: physiotherapy (age relation https://www.jospt.org/doi/pdf/10.2519/jospt.2012.4147)
## categories with (CID) patient follow-up
# 13 [4-055]: Diseases of the musculoskeletal system and connective tissue
#     age relation https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3424705/

# generalized picture of these people
AbsCat<-c(13,23,27,28)
Absentees<-WorkAbs[WorkAbs$Reason.for.absence==AbsCat,]
dim(Absentees)
# [1] 99 21

# 9. challenege your solution
# Create Profiles
# Create Physical Profile of Reoc Absentees --------------------------
Weight<-Absentees$Weight
Height<-Absentees$Height
PhyiscalProfile<-data.frame(Height,Weight)
PhyiscalProfile1<-data.frame(Height,Weight)
PhyiscalProfile$ID<-Absentees$ID
PhyiscalProfile$Reason<-Absentees$Reason.for.absence
PhyiscalProfile$BMI<-Absentees$Body.mass.index
PhyiscalProfile$Age<-Absentees$Age

BMI<-table(Absentees$Body.mass.index)
BMI1<-as.data.frame(BMI)

# comparing euclidean vs manhattan
dist(PhyiscalProfile1,method="euclidean")==
  dist(PhyiscalProfile1,method="manhattan")
PPdistE<-dist(PhyiscalProfile1,method="euclidean")
PPdistM<-dist(PhyiscalProfile1,method="manhattan")
# hierchical clustering
PPhclustE<-hclust(PPdistE)
PPhclustM<-hclust(PPdistM)

# dendrogram comparison
png("Plot2",width=520,height=480)
par(mfrow=c(1,2))
plot(PPhclustE,main="Euclidean Dendrogram")
rect.hclust(PPhclustE,k=6,border="red")
legend("topleft",pch=19,col="red",
       legend="Tree Cut at 6 Clusters",cex=0.7)
plot(PPhclustM,main="Manhattan Dendrogram")
rect.hclust(PPhclustM,k=7,border="red")
legend("topleft",pch=19,col="red",
       legend="Tree Cut at 7 Clusters",cex=0.7)
dev.off()
# cluster comparison
png("Plot3",width=480,height=480)
par(mfrow=c(1,2))
plot(Weight,Height,col=cutree(PPhclustE,k=6),pch=19,
     main="Physical Profile Using \nEuclidean Distance \n(6 Groups)",
     xlab="Weight in Kilograms",
     ylab="Height in Centimeters")
legend("topleft",pch=19,col=as.character(1:6),
       legend=as.character(1:6))
plot(Weight,Height,col=cutree(PPhclustM,k=7),pch=19,
     main="Physical Profile Using \nManhattan Distance \n(7 Groups)",
     xlab="Weight in Kilograms",
     ylab="Height in Centimeters")
legend("topleft",pch=19,col=as.character(1:7),
       legend=as.character(1:7))
dev.off()
# Decided on Manhattan Grouping with 7 Groups
Hclust1<-cutree(PPhclustM,k=7)
PhyiscalProfile$Groups<-Hclust1
Absentees$Groups<-Hclust1
kmM<-kmeans(dist(PhyiscalProfile1,method="manhattan"),centers=7)
Absentees$Grps<-kmM$cluster

# kmeans vs hierchical comp
png("Plot4",width=480,height=480)
par(mfrow=c(1,2))
plot(Absentees$Weight,Absentees$Height,col=Absentees$Grps,pch=19,main="kmeans")
plot(Absentees$Weight,Absentees$Height,col=Absentees$Groups,pch=19,main="hclust")
dev.off()
# decided for hclust and not kmeans
png("Plot5",width=480,height=480)
m2<-rbind(c(1,2),c(1,3))
layout(m2)
par(mar=c(5,4,5,2))
plot(Weight,Height,col=cutree(PPhclustM,k=7),pch=19,
     main="Height & Weight Using 7 Groups",
     xlab="Weight in Kilograms",
     ylab="Height in Centimeters")
legend("topright",pch=19,col=as.character(1:7),
       legend=as.character(1:7))
title(list("Physical Profile for Categories 13,23,27 & 28",cex=1.5,
           col="red",font=6),line=-1,outer=T)
plot(density(PhyiscalProfile$Age),main="Age Distribution")
barplot(BMI1$Freq,names.ar=BMI1$Var1,cex.names=0.7,
        main="Body Mass Index Frequencies",
        xlab="Body Mas Index",
        ylab="Frequencies")
dev.off()

# test clusterred categories for physical profile answers
stackbar1<-with(PhyiscalProfile, table(Reason,Groups))
stackbar2<-stackbar1[c(14,23,27,28),]

png("Plot6",width=480,height=480)
m1<-rbind(c(1,2),c(1,3))
layout(m1)
par(mar=c(5,4,5,2))
barplot(stackbar2,col=as.character(4:8),
        main="Absense Reasons Per Cluster",
        xlab="Clustered Group",
        ylab="Top 10% Reasons For Absenses")
  legend("topright",pch=19,col=as.character(4:8),
       legend=c(
         "musculoskeletal diseases",
         "medical consultation",
         "physiotherapy",
         "dental consultation"),cex=0.8)
title(list("Physical Profile Analysis for 7 Clustered Groups",cex=1.5,
             col="red",font=6),line=-1,outer=T)
boxplot(Absentees$Body.mass.index~Absentees$Groups,
        main="BMI Per Cluster",
        xlab="Clustered Group",
        ylab="Body Mass Index")
boxplot(Absentees$Age~Absentees$Groups,
        main="Age Per Cluster",
        xlab="Clustered Group",
        ylab="Age Range")
dev.off()