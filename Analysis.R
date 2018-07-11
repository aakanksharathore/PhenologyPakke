## Code create by AA on 18 dec 2017 for phenology community wide patterns
library(ggplot2)
# Set working directory
setwd("/media/aakanksha/f41d5ac2-703c-4b56-a960-cd3a54f21cfb/aakanksha/Documents/Backup/PhenologyAnalysis")

#Load data
dat = read.csv("2011_to_2017Data.csv")
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


#################################Part 1###################################################################################################################
#1. Community-wide patterns of flowering (flower buds and flowers): annual variation

#Proportion of flowering trees in each month
datFl=dat[dat$Fl==1 | dat$Fl==0,]
Fl1=tapply(as.numeric(paste(datFl$Fl)),list(datFl$Year,datFl$Month),FUN=sum)
FlT=tapply(as.numeric(paste(datFl$Fl)),list(datFl$Year,datFl$Month),FUN=length)
FlP=Fl1/FlT
flp=data.frame()
#Average over years
for(i in 2:length(colnames(FlP))){
  flp[i-1,1]=mean(FlP[,i],na.rm=TRUE)
  flp[i-1,2]=sd(FlP[,i],na.rm=TRUE)
  flp[i-1,3]=colnames(FlP)[i]
}
#reorder rows to represent julian calender order
flp=flp[match(mms, flp[,3]),]

#Proportion of flower buds in each month
datFlB=dat[dat$FlB==1 | dat$FlB==0,]
Fb1=tapply(as.numeric(paste(datFlB$FlB)),list(datFlB$Year,datFl$Month),FUN=sum)
FbT=tapply(as.numeric(paste(datFlB$FlB)),list(datFlB$Year,datFlB$Month),FUN=length)
FbP=Fb1/FbT
fbp=data.frame()
#Average over years
for(i in 2:length(colnames(FbP))){
  fbp[i-1,1]=mean(FbP[,i],na.rm=TRUE)
  fbp[i-1,2]=sd(FbP[,i],na.rm=TRUE)
  fbp[i-1,3]=colnames(FbP)[i]
}
#reorder rows to represent julian calender order
fbp=fbp[match(mms, fbp[,3]),]
#2. Community-wide patterns of fruiting (unripe and ripe fruits): annual variation

#Proportion of fRUITING trees in each month
datRfr=dat[dat$Rfr==1 | dat$Rfr==0,]
Rfr1=tapply(as.numeric(paste(datRfr$Rfr)),list(datRfr$Year,datRfr$Month),FUN=sum)
RfrT=tapply(as.numeric(paste(datRfr$Rfr)),list(datRfr$Year,datRfr$Month),FUN=length)
RfrP=Rfr1/RfrT
rfr=data.frame()
#Average over years
for(i in 2:length(colnames(RfrP))){
  rfr[i-1,1]=mean(RfrP[,i],na.rm=TRUE)
  rfr[i-1,2]=sd(RfrP[,i],na.rm=TRUE)
  rfr[i-1,3]=colnames(RfrP)[i]
}
#reorder rows to represent julian calender order
rfr=rfr[match(mms, rfr[,3]),]

#Proportion of flower buds in each month
datUfr=dat[dat$Ufr==1 | dat$Ufr==0,]
Ufr1=tapply(as.numeric(paste(datUfr$Ufr)),list(datUfr$Year,datUfr$Month),FUN=sum)
UfrT=tapply(as.numeric(paste(datUfr$Ufr)),list(datUfr$Year,datUfr$Month),FUN=length)
UfrP=Ufr1/UfrT
ufr=data.frame()
#Average over years
for(i in 2:length(colnames(UfrP))){
  ufr[i-1,1]=mean(UfrP[,i],na.rm=TRUE)
  ufr[i-1,2]=sd(UfrP[,i],na.rm=TRUE)
  ufr[i-1,3]=colnames(UfrP)[i]
}
#reorder rows to represent julian calender order
ufr=ufr[match(mms, ufr[,3]),]
#3. Community-wide patterns of leafing (leaf flush, senescent or old leaves, leaf fall (when there are no leaves): annual variation

#Leaf flush (Young leaves)
datYsh=dat[dat$Ysh==1 | dat$Ysh==0,]
Ysh1=tapply(as.numeric(paste(datYsh$Ysh)),list(datYsh$Year,datYsh$Month),FUN=sum)
YshT=tapply(as.numeric(paste(datYsh$Ysh)),list(datYsh$Year,datYsh$Month),FUN=length)
YshP=Ysh1/YshT
ysh=data.frame()
#Average over years
for(i in 2:length(colnames(YshP))){
  ysh[i-1,1]=mean(YshP[,i],na.rm=TRUE)
  ysh[i-1,2]=sd(YshP[,i],na.rm=TRUE)
  ysh[i-1,3]=colnames(YshP)[i]
}
#reorder rows to represent julian calender order
ysh=ysh[match(mms, ysh[,3]),]
#Green Leaves
datlv=dat[dat$Lv==1 | dat$Lv==0,]
lv1=tapply(as.numeric(paste(datlv$Lv)),list(datlv$Year,datlv$Month),FUN=sum)
lvT=tapply(as.numeric(paste(datlv$Lv)),list(datlv$Year,datlv$Month),FUN=length)
lvP=lv1/lvT
lv=data.frame()
#Average over years
for(i in 2:length(colnames(lvP))){
  lv[i-1,1]=mean(lvP[,i],na.rm=TRUE)
  lv[i-1,2]=sd(lvP[,i],na.rm=TRUE)
  lv[i-1,3]=colnames(lvP)[i]
}
#reorder rows to represent julian calender order
lv=lv[match(mms, lv[,3]),]
#No leaves
Nl1=tapply(dat$Lv[dat$Olv=="0" & dat$Lv=="0" & dat$Ysh=="0"],list(dat$Year[dat$Olv=="0" & dat$Lv=="0" & dat$Ysh=="0"],dat$Month[dat$Olv=="0" & dat$Lv=="0" & dat$Ysh=="0"]),FUN=length)
NlT=tapply(dat$Lv,list(dat$Year,dat$Month),FUN=length)
NlP=Nl1/NlT
nlp=data.frame()
#Average over years
for(i in 2:length(colnames(NlP))){
  nlp[i-1,1]=mean(NlP[,i],na.rm=TRUE)
  nlp[i-1,2]=sd(NlP[,i],na.rm=TRUE)
  nlp[i-1,3]=colnames(NlP)[i]
}
#reorder rows to represent julian calender order
nlp=nlp[match(mms, nlp[,3]),]
#Graphs


x=1:12
#1. Community-wide patterns of flowering (flower buds and flowers): annual variation

#Flowers
png("Graphs//CommunityWideFlowering.png",width=1080,height=720,units = "px", pointsize = 20)
plot(x,flp[,1],xaxt="n",xlab="Months",ylab="% trees flowering",type="b",pch=20,col="navy",ylim=c(0,0.5))
axis(1, at=x, labels=flp[,3])
arrows(x, flp[,1]-flp[,2], x, flp[,1]+flp[,2], length=0.05, angle=90, code=3)
dev.off();
#Flower-buds
png("Graphs//CommunityWideFlowerbuds.png",width=1080,height=720,units = "px", pointsize = 20)
plot(x,fbp[,1],xaxt="n",xlab="Months",ylab="% trees with buds",type="b",pch=20,col="navy",ylim=c(0,0.5))
axis(1, at=x, labels=fbp[,3])
arrows(x, fbp[,1]-fbp[,2], x, fbp[,1]+fbp[,2], length=0.05, angle=90, code=3)
dev.off();
#2. Community-wide patterns of fruiting (unripe and ripe fruits): annual variation
png("Graphs//CommunityWideRipefruits.png",width=1080,height=720,units = "px", pointsize = 20)
plot(x,rfr[,1],xaxt="n",xlab="Months",ylab="% trees with ripe fruits",type="b",pch=20,col="navy",ylim=c(0,0.5))
axis(1, at=x, labels=rfr[,3])
arrows(x, rfr[,1]-rfr[,2], x, rfr[,1]+rfr[,2], length=0.05, angle=90, code=3)
dev.off();
png("Graphs//CommunityWideUnripefruits.png",width=1080,height=720,units = "px", pointsize = 20)
plot(x,ufr[,1],xaxt="n",xlab="Months",ylab="% trees with unripe fruits",type="b",pch=20,col="navy",ylim=c(0,0.5))
axis(1, at=x, labels=ufr[,3])
arrows(x, ufr[,1]-ufr[,2], x, ufr[,1]+ufr[,2], length=0.05, angle=90, code=3)
dev.off();

#3. Community-wide patterns of leafing (leaf flush, senescent or old leaves, leaf fall (when there are no leaves): annual variation
png("Graphs//CommunityWideLeafflush.png",width=1080,height=720,units = "px", pointsize = 20)
plot(x,ysh[,1],xaxt="n",xlab="Months",ylab="% trees with leaf flush",type="b",pch=20,col="navy",ylim=c(0,1))
axis(1, at=x, labels=ysh[,3])
arrows(x, ysh[,1]-ysh[,2], x, ysh[,1]+ysh[,2], length=0.05, angle=90, code=3)
dev.off();
png("Graphs//CommunityWideLeaves.png",width=1080,height=720,units = "px", pointsize = 20)
plot(x,lv[,1],xaxt="n",xlab="Months",ylab="% trees with green leaves",type="b",pch=20,col="navy",ylim=c(0,1))
axis(1, at=x, labels=lv[,3])
arrows(x, lv[,1]-lv[,2], x, lv[,1]+lv[,2], length=0.05, angle=90, code=3)
dev.off();
png("Graphs//CommunityWideNoLeaves.png",width=1080,height=720,units = "px", pointsize = 20)
plot(x,nlp[,1],xaxt="n",xlab="Months",ylab="% trees with buds",type="b",pch=20,col="navy",ylim=c(0,1))
axis(1, at=x, labels=nlp[,3])
arrows(x, nlp[,1]-nlp[,2], x, nlp[,1]+nlp[,2], length=0.05, angle=90, code=3)
dev.off();

##############################################################################################################
#######################################part 2################################################################
###############################################################################################################
#dat$datee=paste(dat$Year,dat$Month)
#Function to get proportion
getPr = function(dat1,stg,cr){
  dat=dat1[dat1$Disp.mode==cr,]
  pl=dat[,paste(stg)]
  Pl1=tapply(pl[pl=="1"],list(dat$Year[pl=="1"],dat$Month[pl=="1"]),FUN=length)
  PlT=tapply(pl,list(dat$Year,dat$Month),FUN=length)
  Pl=Pl1/PlT
  return(Pl)
  }
getPr1 = function(dat1,stg,cr){
  dat=dat1[dat1$Disp.mode!=cr,]
  pl=dat[,paste(stg)]
  Pl1=tapply(pl[pl=="1"],list(dat$Year[pl=="1"],dat$Month[pl=="1"]),FUN=length)
  PlT=tapply(pl,list(dat$Year,dat$Month),FUN=length)
  Pl=Pl1/PlT
  return(Pl)
}
#1. Patterns in flowering/fruiting of animal-dispersed vs. mechanically-dispersed (seasonal and annual variation)

FlW=getPr(dat1=dat,stg="Fl",cr="Wind")
FlA=getPr1(dat1=dat,stg="Fl",cr="Wind")
flw=data.frame()
#Average over years
for(i in 2:length(colnames(FlW))){
  flw[i-1,1]=mean(FlW[,i],na.rm=TRUE)
  flw[i-1,2]=sd(FlW[,i],na.rm=TRUE)
  flw[i-1,3]=colnames(FlW)[i]
}
#reorder rows to represent julian calender order
flw=flw[match(mms, flw[,3]),]
fla=data.frame()
#Average over years
for(i in 2:length(colnames(FlA))){
  fla[i-1,1]=mean(FlA[,i],na.rm=TRUE)
  fla[i-1,2]=sd(FlA[,i],na.rm=TRUE)
  fla[i-1,3]=colnames(FlA)[i]
}
#reorder rows to represent julian calender order
fla=fla[match(mms, fla[,3]),]
#Flower buds
FbW=getPr(dat1=dat,stg="FlB",cr="Wind")
FbA=getPr1(dat1=dat,stg="FlB",cr="Wind")

fbw=data.frame()
#Average over years
for(i in 2:length(colnames(FbW))){
  fbw[i-1,1]=mean(FbW[,i],na.rm=TRUE)
  fbw[i-1,2]=sd(FbW[,i],na.rm=TRUE)
  fbw[i-1,3]=colnames(FbW)[i]
}
#reorder rows to represent julian calender order
fbw=fbw[match(mms, fbw[,3]),]
fba=data.frame()
#Average over years
for(i in 2:length(colnames(FbA))){
  fba[i-1,1]=mean(FbA[,i],na.rm=TRUE)
  fba[i-1,2]=sd(FbA[,i],na.rm=TRUE)
  fba[i-1,3]=colnames(FbA)[i]
}
#reorder rows to represent julian calender order
fba=fba[match(mms, fba[,3]),]
#Fruiting
FrW=getPr(dat1=dat,stg="Rfr",cr="Wind")
FrA=getPr1(dat1=dat,stg="Rfr",cr="Wind")

frw=data.frame()
#Average over years
for(i in 2:length(colnames(FrW))){
  frw[i-1,1]=mean(FrW[,i],na.rm=TRUE)
  frw[i-1,2]=sd(FrW[,i],na.rm=TRUE)
  frw[i-1,3]=colnames(FrW)[i]
}
#reorder rows to represent julian calender order
frw=frw[match(mms, frw[,3]),]
fra=data.frame()
#Average over years
for(i in 2:length(colnames(FrA))){
  fra[i-1,1]=mean(FrA[,i],na.rm=TRUE)
  fra[i-1,2]=sd(FrA[,i],na.rm=TRUE)
  fra[i-1,3]=colnames(FrA)[i]
}
#reorder rows to represent julian calender order
fra=fra[match(mms, fra[,3]),]

#2. Patterns in flowering/fruiting of bird-dispersed and mammal-dispersed (seasonal and annual variation)
FlB=getPr(dat1=dat,stg="Fl",cr="Bird-dispersed")
FlM=getPr(dat1=dat,stg="Fl",cr="Mammal")

flb=data.frame()
#Average over years
for(i in 2:length(colnames(FlB))){
  flb[i-1,1]=mean(FlB[,i],na.rm=TRUE)
  flb[i-1,2]=sd(FlB[,i],na.rm=TRUE)
  flb[i-1,3]=colnames(FlB)[i]
}
#reorder rows to represent julian calender order
flb=flb[match(mms, flb[,3]),]
flm=data.frame()
#Average over years
for(i in 2:length(colnames(FlM))){
  flm[i-1,1]=mean(FlM[,i],na.rm=TRUE)
  flm[i-1,2]=sd(FlM[,i],na.rm=TRUE)
  flm[i-1,3]=colnames(FlM)[i]
}
#reorder rows to represent julian calender order
flm=flm[match(mms, flm[,3]),]
#Flower buds
FbB=getPr(dat1=dat,stg="FlB",cr="Bird-dispersed")
FbM=getPr(dat1=dat,stg="FlB",cr="Mammal")

fbb=data.frame()
#Average over years
for(i in 2:length(colnames(FbB))){
  fbb[i-1,1]=mean(FbB[,i],na.rm=TRUE)
  fbb[i-1,2]=sd(FbB[,i],na.rm=TRUE)
  fbb[i-1,3]=colnames(FbB)[i]
}
#reorder rows to represent julian calender order
fbb=fbb[match(mms, fbb[,3]),]
fbm=data.frame()
#Average over years
for(i in 2:length(colnames(FbM))){
  fbm[i-1,1]=mean(FbM[,i],na.rm=TRUE)
  fbm[i-1,2]=sd(FbM[,i],na.rm=TRUE)
  fbm[i-1,3]=colnames(FbM)[i]
}
#reorder rows to represent julian calender order
fbm=fbm[match(mms, fbm[,3]),]

#Fruiting
FrB=getPr(dat1=dat,stg="Rfr",cr="Bird-dispersed")
FrM=getPr(dat1=dat,stg="Rfr",cr="Mammal")

frb=data.frame()
#Average over years
for(i in 2:length(colnames(FrB))){
  frb[i-1,1]=mean(FrB[,i],na.rm=TRUE)
  frb[i-1,2]=sd(FrB[,i],na.rm=TRUE)
  frb[i-1,3]=colnames(FrB)[i]
}
#reorder rows to represent julian calender order
frb=frb[match(mms, frb[,3]),]
frm=data.frame()
#Average over years
for(i in 2:length(colnames(FrM))){
  frm[i-1,1]=mean(FrM[,i],na.rm=TRUE)
  frm[i-1,2]=sd(FrM[,i],na.rm=TRUE)
  frm[i-1,3]=colnames(FrM)[i]
}
#reorder rows to represent julian calender order
frm=frm[match(mms, frm[,3]),]

#Graphs

#1. Animal vs. Machsanical

#Flowers
png("Graphs//AnimalvsMechsanicalFlowering.png",width=1080,height=720,units = "px", pointsize = 20)
plot(x,flw[,1],xaxt="n",xlab="Months",ylab="% trees flowering",type="b",pch=20,col="navy",ylim=c(0,0.5))
arrows(x, flw[,1]-flw[,2], x, flw[,1]+flw[,2], length=0.05, angle=90, code=3,col="navy")
points(x,fla[,1],xaxt="n",xlab="Months",ylab="% trees flowering",type="b",pch=8,col="orange",lty=6,ylim=c(0,0.5))
arrows(x, fla[,1]-fla[,2], x, fla[,1]+fla[,2], length=0.05, angle=90,col="orange",code=3)
axis(1, at=x, labels=flw[,3])
legend("topright", legend=c("Wind-dispersed", "Animal-dispersed"),col=c("navy", "orange"), lty=c(1,6),pch=c(20,8), cex=0.8)
dev.off();
#Flower-buds
png("Graphs//AnimalvsMechsanicalFlowerbuds.png",width=1080,height=720,units = "px", pointsize = 20)
plot(x,fbw[,1],xaxt="n",xlab="Months",ylab="% trees flowering",type="b",pch=20,col="navy",ylim=c(0,0.5))
arrows(x, fbw[,1]-flw[,2], x, fbw[,1]+fbw[,2], length=0.05, angle=90, code=3,col="navy")
points(x,fba[,1],xaxt="n",xlab="Months",ylab="% trees flowering",type="b",pch=8,col="orange",lty=6,ylim=c(0,0.5))
arrows(x, fba[,1]-fba[,2], x, fba[,1]+fba[,2], length=0.05, angle=90,col="orange",code=3)
axis(1, at=x, labels=fbw[,3])
legend("topright", legend=c("Wind-dispersed", "Animal-dispersed"),col=c("navy", "orange"), lty=c(1,6),pch=c(20,8), cex=0.8)
dev.off();
#ripe fruits
png("Graphs//AnimalvsMechsanicalFruiting.png",width=1080,height=720,units = "px", pointsize = 20)
plot(x,frw[,1],xaxt="n",xlab="Months",ylab="% trees flowering",type="b",pch=20,col="navy",ylim=c(0,0.5))
arrows(x, frw[,1]-frw[,2], x, frw[,1]+frw[,2], length=0.05, angle=90, code=3,col="navy")
points(x,fra[,1],xaxt="n",xlab="Months",ylab="% trees flowering",type="b",pch=8,col="orange",lty=6,ylim=c(0,0.5))
arrows(x, fra[,1]-fra[,2], x, fra[,1]+fra[,2], length=0.05, angle=90,col="orange",code=3)
axis(1, at=x, labels=frw[,3])
legend("topright", legend=c("Wind-dispersed", "Animal-dispersed"),col=c("navy", "orange"), lty=c(1,6),pch=c(20,8), cex=0.8)
dev.off();

#1. Birds vs Mammals

#Flowers
png("Graphs//AnimalvsMechsanicalFlowering.png",width=1080,height=720,units = "px", pointsize = 20)
plot(x,flw[,1],xaxt="n",xlab="Months",ylab="% trees flowering",type="b",pch=20,col="navy",ylim=c(0,0.5))
arrows(x, flw[,1]-flw[,2], x, flw[,1]+flw[,2], length=0.05, angle=90, code=3,col="navy")
points(x,fla[,1],xaxt="n",xlab="Months",ylab="% trees flowering",type="b",pch=8,col="orange",lty=6,ylim=c(0,0.5))
arrows(x, fla[,1]-fla[,2], x, fla[,1]+fla[,2], length=0.05, angle=90,col="orange",code=3)
axis(1, at=x, labels=flw[,3])
legend("topright", legend=c("Wind-dispersed", "Animal-dispersed"),col=c("navy", "orange"), lty=c(1,6),pch=c(20,8), cex=0.8)
dev.off();
#Flower-buds
png("Graphs//AnimalvsMechsanicalFlowerbuds.png",width=1080,height=720,units = "px", pointsize = 20)
plot(x,fbw[,1],xaxt="n",xlab="Months",ylab="% trees flowering",type="b",pch=20,col="navy",ylim=c(0,0.5))
arrows(x, fbw[,1]-flw[,2], x, fbw[,1]+fbw[,2], length=0.05, angle=90, code=3,col="navy")
points(x,fba[,1],xaxt="n",xlab="Months",ylab="% trees flowering",type="b",pch=8,col="orange",lty=6,ylim=c(0,0.5))
arrows(x, fba[,1]-fba[,2], x, fba[,1]+fba[,2], length=0.05, angle=90,col="orange",code=3)
axis(1, at=x, labels=fbw[,3])
legend("topright", legend=c("Wind-dispersed", "Animal-dispersed"),col=c("navy", "orange"), lty=c(1,6),pch=c(20,8), cex=0.8)
dev.off();
#ripe fruits
png("Graphs//AnimalvsMechsanicalFruiting.png",width=1080,height=720,units = "px", pointsize = 20)
plot(x,frw[,1],xaxt="n",xlab="Months",ylab="% trees flowering",type="b",pch=20,col="navy",ylim=c(0,0.5))
arrows(x, frw[,1]-frw[,2], x, frw[,1]+frw[,2], length=0.05, angle=90, code=3,col="navy")
points(x,fra[,1],xaxt="n",xlab="Months",ylab="% trees flowering",type="b",pch=8,col="orange",lty=6,ylim=c(0,0.5))
arrows(x, fra[,1]-fra[,2], x, fra[,1]+fra[,2], length=0.05, angle=90,col="orange",code=3)
axis(1, at=x, labels=frw[,3])
legend("topright", legend=c("Wind-dispersed", "Animal-dispersed"),col=c("navy", "orange"), lty=c(1,6),pch=c(20,8), cex=0.8)
dev.off();


##################################################################################################################################
#Analysis of patterns of fruiting based on fruit type (seasonal and annual variation)-One graph
##################################                                      ##################################################


FrTp=unique(dat$Fruit.type)

#Tabulate for month,year and fruit types

#Green Leaves
datft=dat[dat$Rfr==1 | dat$Rfr==0,]
ft1=tapply(as.numeric(paste(datft$Rfr)),list(datft$Year,datft$Month,datft$Fruit.type),FUN=sum)
ftT=tapply(as.numeric(paste(datft$Rfr)),list(datft$Year,datft$Month,datft$Fruit.type),FUN=length)
ftP=ft1/ftT

#Average across years
ftp=data.frame()
for(i in 2:length(colnames(ftP))){
  ftp[i-1,1,]=mean(FrM[,i],na.rm=TRUE)
  frm[i-1,2]=sd(FrM[,i],na.rm=TRUE)
  frm[i-1,3]=colnames(FrM)[i]
}

#Graph



