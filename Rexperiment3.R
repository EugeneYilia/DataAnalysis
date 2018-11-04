df <- read.csv("/home/eugeneliu/RProjects/experiment/sources/death rate.csv",header = TRUE)
lengths(df) 
sum(is.na(df))
lengths(df)
length(which(df$q_male<=0))+length(which(df$q_male>1))
na.omit(df)
library(sqldf)
df <- sqldf("select * from df where q_male>0 and q_male<=1")
df

attach(df)
plot(Age,log(q_male),main="age and death",xlab = "Age",ylab = "Death")
detach(df)
attach(df)
plot(Year,log(q_male),main="year and death",xlab = "Year",ylab = "Death")
detach(df)

attach(df)
plot(Age,log(Female_Exp+Male_Exp),main = "Age and live people",xlab = "Age",ylab = "Live People")
detach(df)

hist(df$Male_death)

hist(log(df$Male_death))

library(corrgram)
corrgram(df,order=TRUE,
         lower.panel = panel.shade,
         upper.panel = panel.pie,
         text.panel = panel.txt,
         main = "Corrgram of df data")
         


houseIndex <- read.csv("/home/eugeneliu/RProjects/experiment/sources/House-handle.csv",header = TRUE)

plot(as.Date(houseIndex$date,"%d-%b-%y"),houseIndex$index,main="the variety of HPI",xlab= "date",ylab="HPI")

originData <- c(1,houseIndex[1:dim(houseIndex)[1]-1,]$index)
plot(as.Date(houseIndex$date,"%d-%b-%y"),houseIndex$index-originData,main="the variety of delta-HPI",xlab= "date",ylab="delta-HPI")

 

originData <- c(1,houseIndex[1:dim(houseIndex)[1]-1,]$index)
differ <- houseIndex$index-originData
houseIndex$differ <- differ
upperData <- houseIndex[houseIndex$differ>=0,]

lowerData <- houseIndex[houseIndex$differ<0,]
plot(as.Date(upperData$date,"%d-%b-%y"),upperData$differ,ylim = c(-0.1,0.1),main="the variety of delta-HPI",xlab= "date",ylab="delta-HPI",pch=3)
par(new=T) 
plot(as.Date(lowerData$date,"%d-%b-%y"),xaxt="n",lowerData$differ,ylim= c(-0.1,0.1),main="the variety of delta-HPI",xlab= "date",ylab="delta-HPI",pch=1)
abline(h=c(0),lwd=1.5,lty=2,col="gray")


DT <- xtabs(round(differ,4)~month+year,data=houseIndex)
DT

houseIndex$index[houseIndex$index==0]<-NA
houseIndex$index
aggregate(houseIndex$index,by=list(houseIndex$year),FUN=mean)
aggregate(houseIndex$index,by=list(houseIndex$month),FUN=mean)

boxplot(houseIndex$index,main="HPI delta variety")























