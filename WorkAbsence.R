#!/bin/bash
    
# load library & data
# all plots and work is done in base R
WorkAbs<-read.csv("Absenteeism_at_work.csv",
    sep=";",header=T)

# build "mode" function to determine most frequent 
# reason for absence
WorkAbs$Reason.for.absence<-as.factor(WorkAbs$Reason.for.absence)
getmode<-function(v){
   uniqv<-unique(v)
   uniqv[which.max(tabulate(match(v,uniqv)))]
}
getmode(WorkAbs$Reason.for.absence)

# build frequency table for absence reasons
AbsReasons1<-table(WorkAbs$Reason.for.absence)
AbsReasons2<-as.data.frame(AbsReasons1)
AbsReasons2Var1<-as.character(AbsReasons2$Var1)

# (Plot1) initial discovery plots
png("Plot1.png",width=480,height=480)
m<-rbind(c(1,1,1),c(2,3,4))
layout(m)
par(mar=c(5,5,5,2),len=1)
boxplot(WorkAbs$Absenteeism.time.in.hours~
        WorkAbs$Reason.for.absence,
        main="Work Hours Missed by Category",
        xlab="Reasons for Absence by Category",
        ylab="Work Hours Missed",cex.names=0.8)
title(list("Initial Discovery of The Reason for Abseentism by 4 Plots",
        cex=1.5,col="red",font = 6),line=-1.5,outer=T)
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

# create sample containing categories 13,23, 27 & 28
# with sum of 99 observations 
AbsCat<-c(13,23,27,28)
Absentees<-WorkAbs[WorkAbs$Reason.for.absence==AbsCat,]

# 9. challenege your solution
# Create Physical Profiles of Reocurring Absentees
Weight<-Absentees$Weight
Height<-Absentees$Height
PhysicalProfile<-data.frame(Height,Weight)
PhysicalProfile1<-data.frame(Height,Weight)
PhysicalProfile$ID<-Absentees$ID
PhysicalProfile$Reason<-Absentees$Reason.for.absence
PhysicalProfile$BMI<-Absentees$Body.mass.index
PhysicalProfile$Age<-Absentees$Age
BMI<-table(Absentees$Body.mass.index)
BMI1<-as.data.frame(BMI)

# comparing euclidean distances vs manhattan distances
if(all(dist(PhysicalProfile1,method="euclidean")==
  dist(PhysicalProfile1,method="manhattan"))==TRUE){
        message("Euclidean Distances == Manhattan Distances")
}else{
        message("Euclidean Distances != Manhattan Distances")
}
PPdistE<-dist(PhysicalProfile1,method="euclidean")
PPdistM<-dist(PhysicalProfile1,method="manhattan")

# hierchical clustering linkage comparison
PPhclustEC<-hclust(PPdistE,"complete")
PPhclustEA<-hclust(PPdistE,"average")
PPhclustES<-hclust(PPdistE,"single")
PPhclustMC<-hclust(PPdistM,"complete")
PPhclustMA<-hclust(PPdistM,"average")
PPhclustMS<-hclust(PPdistM,"single")

# euclidean/ manhattan dendrogram comparison
png("Plot2.png",width=520,height=480)
par(mfrow=c(2,3))
plot(PPhclustEC,main="Euclidean \nComplete Linkage")
rect.hclust(PPhclustEC,k=7,border="red")
plot(PPhclustEA,main="Euclidean \nAverage Linkage")
rect.hclust(PPhclustEA,k=7,border="red")
plot(PPhclustES,main="Euclidean \nSingle Linkage")
rect.hclust(PPhclustES,k=7,border="red")
plot(PPhclustMC,main="Manhattan \nComplete Linkage")
rect.hclust(PPhclustMC,k=7,border="red")
plot(PPhclustMA,main="Manhattan \nAverage Linkage")
rect.hclust(PPhclustMA,k=7,border="red")
plot(PPhclustMS,main="Manhattan \nSingle Linkage")
rect.hclust(PPhclustMS,k=7,border="red")
dev.off()

# euclidean/ manhattan cluster comparison using complete linkage
png("Plot3.png",width=520,height=480)
par(mfrow=c(1,2))
plot(Weight,Height,col=cutree(PPhclustEC,k=7),pch=19,
     main="Physical Profile Using \nEuclidean Distance &\nComplete Linkage",
     xlab="Weight in Kilograms",
     ylab="Height in Centimeters")
legend("topleft",pch=19,col=as.character(1:7),
       legend=as.character(1:7))
plot(Weight,Height,col=cutree(PPhclustMC,k=7),pch=19,
     main="Physical Profile Using \nManhattan Distance &\nComplete Linkage",
     xlab="Weight in Kilograms",
     ylab="Height in Centimeters")
legend("topleft",pch=19,col=as.character(1:7),
       legend=as.character(1:7))
dev.off()

# Build Hclust and Kmeans Data
Hclust1<-cutree(PPhclustMC,k=7)
PhysicalProfile$Groups<-Hclust1
Absentees$Groups<-Hclust1
kmM<-kmeans(dist(PhysicalProfile1,
        method="manhattan"),centers=7,nstart=50)
Absentees$Grps<-kmM$cluster

# kmeans vs hierchical cluster comparisons
png("Plot4.png",width=520,height=480)
par(mfrow=c(1,2))
plot(Absentees$Weight,Absentees$Height,col=kmM$cluster,pch=19,main="kmeans")
plot(Absentees$Weight,Absentees$Height,col=Absentees$Groups,pch=19,main="hclust")
dev.off()

# build physical profile for categories 13, 23, 27 & 28
png("Plot5.png",width=520,height=480)
m2<-rbind(c(1,2),c(1,3))
layout(m2)
par(mar=c(5,4,5,2))
plot(Weight,Height,col=cutree(PPhclustMC,k=7),pch=19,
     main="Height & Weight Using 7 Clusters",
     xlab="Weight in Kilograms",
     ylab="Height in Centimeters")
legend("topright",pch=19,col=as.character(1:7),
       legend=as.character(1:7))
title(list("Physical Profile for Categories 13, 23, 27 & 28",
        cex=1.5,col="red",font=6),line=-1,outer=T)
plot(density(PhysicalProfile$Age),main="Age Distribution")
barplot(BMI1$Freq,names.ar=BMI1$Var1,cex.names=0.7,
        main="Body Mass Index Frequencies",
        xlab="Body Mas Index",
        ylab="Frequencies")
dev.off()

# test clustered categories for physical profile answers
stackbar1<-with(PhysicalProfile, table(Reason,Groups))
stackbar2<-stackbar1[c(14,23,27,28),]

png("Plot6.png",width=520,height=480)
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