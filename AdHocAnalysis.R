## Code create by AA on 18 July 2018 for phenology analysis NCF annual meet
library(ggplot2)
# Set working directory
setwd("/media/aakanksha/f41d5ac2-703c-4b56-a960-cd3a54f21cfb/aakanksha/Documents/Backup/PhenologyAnalysis")

#Load data
dat = read.csv("2011 to 2017- checked and corrected.csv")
str(dat)
#Clean-up data
dat = na.omit(dat)
#Remove figs and individuals for which less than 4 trees are available and dead trees
countT=table(dat$Scientific.name[dat$Year=="2011" & dat$Month=="Jan"])
dat =dat[-((dat$Scientific.name %in% names(which(countT<4))) | (dat$Dead==1)),]

# Calender ordering of months
mms = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
mms=factor(mms,levels=mms)

yrs=unique(dat$Year)
nyr=length(yrs)
dat$date=paste(dat$Year,dat$Month)

lengthOFunique <- function(x) {
 length(unique(x)) 
}
####Number of species in fruits each month

datfr=dat[dat$Rfr==1 | dat$Rfr==0,]
datRfr=dat[dat$Rfr==1 ,]
FrSp=tapply(datRfr$Scientific.name,list(datRfr$Year,datRfr$Month),FUN=lengthOFunique )
#TSp=tapply(datfr$Scientific.name,list(datfr$Year,datfr$Month),FUN=lengthOFunique )
#FrSP=(FrSp/TSp)*100

####Number of species in leaf flush each month

datl=dat[dat$Ysh==1 | dat$Ysh==0,]
datnl=dat[dat$Ysh==1 ,]
Lf=tapply(datnl$Scientific.name,list(datnl$Year,datnl$Month),FUN=lengthOFunique )
#TSp=tapply(datfr$Scientific.name,list(datfr$Year,datfr$Month),FUN=lengthOFunique )
#FrSP=(FrSp/TSp)*100

####Number of species in flower and buds each month

dat$Date=paste(dat$Month,dat$Year)
datF=datF[datF$Fl==1 | datF$FlB==1,]

Fl=tapply(datF$Scientific.name,list(datF$Year,datF$Month),FUN=lengthOFunique )


#Graphs

#1. Animal vs. Machsanical

#Flowers
png("Graphs//AnimalvsMechanical//Flowering.png",width=1080,height=720,units = "px", pointsize = 20)
plot(x,flw[,1],xaxt="n",xlab="Months",ylab="% trees flowering",type="b",pch=20,col="navy",ylim=c(0,0.5))
arrows(x, flw[,1]-flw[,2], x, flw[,1]+flw[,2], length=0.05, angle=90, code=3,col="navy")
points(x,fla[,1],xaxt="n",xlab="Months",ylab="% trees flowering",type="b",pch=8,col="orange",lty=6,ylim=c(0,0.5))
arrows(x, fla[,1]-fla[,2], x, fla[,1]+fla[,2], length=0.05, angle=90,col="orange",code=3)
axis(1, at=x, labels=flw[,3])
legend("topright", legend=c("Wind-dispersed", "Animal-dispersed"),col=c("navy", "orange"), lty=c(1,6),pch=c(20,8), cex=0.8)
dev.off();
#Flower-buds
png("Graphs//AnimalvsMechanical//Flowerbuds.png",width=1080,height=720,units = "px", pointsize = 20)
plot(x,fbw[,1],xaxt="n",xlab="Months",ylab="% trees with flower buds",type="b",pch=20,col="navy",ylim=c(0,0.5))
arrows(x, fbw[,1]-fbw[,2], x, fbw[,1]+fbw[,2], length=0.05, angle=90, code=3,col="navy")
points(x,fba[,1],xaxt="n",xlab="Months",ylab="",type="b",pch=8,col="orange",lty=6,ylim=c(0,0.5))
arrows(x, fba[,1]-fba[,2], x, fba[,1]+fba[,2], length=0.05, angle=90,col="orange",code=3)
axis(1, at=x, labels=fbw[,3])
legend("topright", legend=c("Wind-dispersed", "Animal-dispersed"),col=c("navy", "orange"), lty=c(1,6),pch=c(20,8), cex=0.8)
dev.off();
#ripe fruits
png("Graphs//AnimalvsMechanical//Fruiting.png",width=1080,height=720,units = "px", pointsize = 20)
plot(x,frw[,1],xaxt="n",xlab="Months",ylab="% trees fruiting",type="b",pch=20,col="navy",ylim=c(0,0.5))
arrows(x, frw[,1]-frw[,2], x, frw[,1]+frw[,2], length=0.05, angle=90, code=3,col="navy")
points(x,fra[,1],xaxt="n",xlab="Months",ylab="",type="b",pch=8,col="orange",lty=6,ylim=c(0,0.5))
arrows(x, fra[,1]-fra[,2], x, fra[,1]+fra[,2], length=0.05, angle=90,col="orange",code=3)
axis(1, at=x, labels=frw[,3])
legend("topright", legend=c("Wind-dispersed", "Animal-dispersed"),col=c("navy", "orange"), lty=c(1,6),pch=c(20,8), cex=0.8)
dev.off();

#1. Birds vs Mammals

#Flowers

png("Graphs//BirdsFlowering.png",width=1080,height=720,units = "px", pointsize = 20)
plot(x,flb[,1],xaxt="n",xlab="Months",ylab="% trees in flower",type="b",pch=20,col="navy",ylim=c(0,30))
arrows(x, flb[,1]-flb[,2], x, flb[,1]+flb[,2], length=0.05, angle=90, code=3,col="orange")
axis(1, at=x, labels=flb[,3])
dev.off();

png("Graphs//MammalsFlowering.png",width=1080,height=720,units = "px", pointsize = 20)
plot(x,flm[,1],xaxt="n",xlab="Months",ylab="% trees in flower",type="b",pch=20,col="navy",ylim=c(0,50))
arrows(x, flm[,1]-flm[,2], x, flm[,1]+flm[,2], length=0.05, angle=90, code=3,col="orange")
axis(1, at=x, labels=flm[,3])
dev.off();

png("Graphs//WindFlowering.png",width=1080,height=720,units = "px", pointsize = 20)
plot(x,flw[,1],xaxt="n",xlab="Months",ylab="% trees in flower",type="b",pch=20,col="navy",ylim=c(0,50))
arrows(x, flw[,1]-flw[,2], x, flw[,1]+flw[,2], length=0.05, angle=90, code=3,col="orange")
axis(1, at=x, labels=flw[,3])
dev.off();

#Fruits

png("Graphs//BirdsFruiting.png",width=1080,height=720,units = "px", pointsize = 20)
plot(x,frb[,1],xaxt="n",xlab="Months",ylab="% trees in fruits",type="b",pch=20,col="navy",ylim=c(0,30))
arrows(x, frb[,1]-frb[,2], x, frb[,1]+frb[,2], length=0.05, angle=90, code=3,col="orange")
axis(1, at=x, labels=frb[,3])
dev.off();

png("Graphs//MammalsFruiting.png",width=1080,height=720,units = "px", pointsize = 20)
plot(x,frm[,1],xaxt="n",xlab="Months",ylab="% trees in fruits",type="b",pch=20,col="navy",ylim=c(0,30))
arrows(x, frm[,1]-frm[,2], x, frm[,1]+frm[,2], length=0.05, angle=90, code=3,col="orange")
axis(1, at=x, labels=frm[,3])
dev.off();

png("Graphs//WindFruiting.png",width=1080,height=720,units = "px", pointsize = 20)
plot(x,frw[,1],xaxt="n",xlab="Months",ylab="% trees in fruits",type="b",pch=20,col="navy",ylim=c(0,50))
arrows(x, frw[,1]-frw[,2], x, frw[,1]+frw[,2], length=0.05, angle=90, code=3,col="orange")
axis(1, at=x, labels=frw[,3])
dev.off()

######Correlations


