####################################################################################################
###############################       PART 3      #################################################
###################################################################################################
# Weather data correlation with phenology patterns

#Would also need it separately for Animal-dispersed, Bird-dispersed, Wind-dispersed.

##Convert phenology to matrix to a dataframe with same key as weather data frame i.e. Month Year
len=nrow(RfrP)*ncol(RfrP)
dRfr = data.frame(Date=rep(NA, length=len),Rfr=rep(NA, length=len))

##Insert Ripe fruit percentages in new data frame
k=1
for(i in 1:nrow(RfrP)){
  for(j in 1:ncol(RfrP)){
    dRfr$Date[k]=paste(colnames(RfrP)[j],rownames(RfrP)[i])
    dRfr$Rfr[k]=RfrP[i,j]*100
    k=k+1
  }
  
}

##Insert flowering percentages

#Proportion of flowering trees in each month (both flowers and flower buds together)
dat$Date=paste(dat$Month,dat$Year)
datF=dat[dat$Fl==1 | dat$Fl==0 | dat$FlB==1 | dat$FlB==0,]
datF$FL=rep(0,nrow(datF))
datF$FL[datF$Fl==1 | datF$FlB==1]=1
FT=tapply(as.numeric(paste(datF$Fl)),list(datF$Date),FUN=length)
F1=tapply(as.numeric(paste(datF$FL)),list(datF$Date),FUN=sum)
FP=(F1/FT)*100

len=nrow(FP)
dFl = data.frame(Date=rep(NA, length=len),Rfr=rep(NA, length=len))

dFl = cbind(Date=names(FP),Fl=as.numeric(FP))


##Correlations
## This part will use data generated from code AnalysisP4.R (weather summary code)
##Temperature
TempMin
TempMax
dTMin=cbind(Date=names(TempMin),Value=as.numeric(TempMin))
dTMax=cbind(Date=names(TempMax),Value=as.numeric(TempMax))

##Total rainfall and last 6 month rainfall
TRain
TSRain=append(rep("NA",6),TSRain)
dTRain=cbind(Date=names(TRain),TRain=as.numeric(TRain),TSRain)

#Daylength
AvgDL
DL=cbind(Date=names(AvgDL),Value=as.numeric(AvgDL))

## 1.1 Fruiting with Total Rainfall

Fr_TR = merge(dRfr,dTRain,by="Date")
corFr_TR=cor.test(as.numeric(paste(Fr_TR$TRain)),Fr_TR$Rfr)

png("Graphs//Birds//CorrFr_TR.png",width=1080,height=720,units = "px", pointsize = 20)
plot(as.numeric(paste(Fr_TR$TRain)),Fr_TR$Rfr,ylab="% trees in ripe fruit",xlab="Total monthly rainfall",pch=20,col="navy")
abline(lm(Fr_TR$Rfr~as.numeric(paste(Fr_TR$TRain))), col="red")
mtext(paste(" p-value = ",round(corFr_TR$p.value,3)))
dev.off()


#1.2 Flowerinf with Total Rainfall
Fl_TR = merge(dFl,dTRain,by="Date")
corFl_TR=cor.test(as.numeric(paste(Fl_TR$TRain)),as.numeric(paste(Fl_TR$Fl)))

png("Graphs//Birds//CorrFl_TR.png",width=1080,height=720,units = "px", pointsize = 20)
plot(as.numeric(paste(Fl_TR$TRain)),as.numeric(paste(Fl_TR$Fl)),ylab="% trees in flower",xlab="Total monthly rainfall",pch=20,col="navy")
abline(lm(as.numeric(paste(Fl_TR$Fl))~as.numeric(paste(Fl_TR$TRain))), col="red")
mtext(paste(" p-value = ",round(corFl_TR$p.value,3)))
dev.off()

## 2.1 Fruiting with last 6 months Rainfall
corFr_TS=cor.test(as.numeric(paste(Fr_TR$TSRain)),Fr_TR$Rfr)

png("Graphs//Birds//CorrFr_TS.png",width=1080,height=720,units = "px", pointsize = 20)
plot(as.numeric(paste(Fr_TR$TSRain)),as.numeric(paste(Fr_TR$Rfr)),ylab="% trees in fruits",xlab="Last 6 months rainfall",pch=20,col="navy")
abline(lm(as.numeric(paste(Fr_TR$Rfr))~as.numeric(paste(Fr_TR$TSRain))), col="red")
mtext(paste(" p-value = ",round(corFr_TS$p.value,3)))
dev.off()
#2.2 Flowerinf with past 6 months Rainfall
corFl_TS=cor.test(as.numeric(paste(Fl_TR$TSRain)),as.numeric(paste(Fl_TR$Fl)))

png("Graphs//Birds//CorrFl_TS.png",width=1080,height=720,units = "px", pointsize = 20)
plot(as.numeric(paste(Fl_TR$TSRain)),as.numeric(paste(Fl_TR$Fl)),ylab="% trees in flower",xlab="Last 6 months rainfall",pch=20,col="navy")
abline(lm(as.numeric(paste(Fl_TR$Fl))~as.numeric(paste(Fl_TR$TSRain))), col="red")
mtext(paste(" p-value = ",round(corFl_TS$p.value)))
dev.off()

## 3.1 Fruiting with monthly min-max temperature

Fr_Tmin = merge(dRfr,dTMin,by="Date")
corFr_Tmin=cor.test(as.numeric(paste(Fr_Tmin$Value)),Fr_Tmin$Rfr)

png("Graphs//Birds//CorrFr_Tmin.png",width=1080,height=720,units = "px", pointsize = 20)
plot(as.numeric(paste(Fr_Tmin$Value)),as.numeric(paste(Fr_Tmin$Rfr)),ylab="% trees in fruits",xlab="Minimum monthly temperature",pch=20,col="navy")
abline(lm(as.numeric(paste(Fr_Tmin$Rfr))~as.numeric(paste(Fr_Tmin$Value))), col="red")
mtext(paste(" p-value = ",round(corFr_Tmin$p.value,3)))
dev.off()

Fr_Tmax = merge(dRfr,dTMax,by="Date")
corFr_Tmax=cor.test(as.numeric(paste(Fr_Tmax$Value)),Fr_Tmax$Rfr)

png("Graphs//Birds//CorrFr_Tmax.png",width=1080,height=720,units = "px", pointsize = 20)
plot(as.numeric(paste(Fr_Tmax$Value)),as.numeric(paste(Fr_Tmax$Rfr)),ylab="% trees in fruits",xlab="Maximum monthly temperature",pch=20,col="navy")
abline(lm(as.numeric(paste(Fr_Tmax$Rfr))~as.numeric(paste(Fr_Tmax$Value))), col="red")
mtext(paste(" p-value = ",round(corFr_Tmax$p.value,3)))
dev.off()

#3.2 Flowerinf with Total Rainfall
Fl_Tmin = merge(dFl,dTMin,by="Date")
corFl_Tmin=cor.test(as.numeric(paste(Fl_Tmin$Value)),as.numeric(Fl_Tmin$Fl))

png("Graphs//Birds//CorrFl_Tmin.png",width=1080,height=720,units = "px", pointsize = 20)
plot(as.numeric(paste(Fl_Tmin$Value)),as.numeric(paste(Fl_Tmin$Fl)),ylab="% trees in flower",xlab="Minimum monthly temperature",pch=20,col="navy")
abline(lm(as.numeric(paste(Fl_Tmin$Fl))~as.numeric(paste(Fl_Tmin$Value))), col="red")
mtext(paste(" p-value = ",round(corFl_Tmin$p.value,3)))
dev.off()

Fl_Tmax = merge(dFl,dTMax,by="Date")
corFl_Tmax=cor.test(as.numeric(paste(Fl_Tmax$Value)),as.numeric(Fl_Tmax$Fl))

png("Graphs//Birds//CorrFl_Tmax.png",width=1080,height=720,units = "px", pointsize = 20)
plot(as.numeric(paste(Fl_Tmax$Value)),as.numeric(paste(Fl_Tmax$Fl)),ylab="% trees in flowers",xlab="Maximum monthly temperature",pch=20,col="navy")
abline(lm(as.numeric(paste(Fl_Tmax$Fl))~as.numeric(paste(Fl_Tmax$Value))), col="red")
mtext(paste(" p-value = ",round(corFl_Tmax$p.value,3)))
dev.off()

## 4.1 Fruiting with Daylength

Fr_DL = merge(dRfr,DL,by="Date")
corFr_DL=cor.test(as.numeric(paste(Fr_DL$Value)),as.numeric(paste(Fr_DL$Rfr)))

png("Graphs//Birds//CorrFr_DL.png",width=1080,height=720,units = "px", pointsize = 20)
plot(as.numeric(paste(Fr_DL$Value)),as.numeric(paste(Fr_DL$Rfr)),ylab="% trees in fruits",xlab="Daylength (hrs)",pch=20,col="navy")
abline(lm(as.numeric(paste(Fr_DL$Rfr))~as.numeric(paste(Fr_DL$Value))), col="red")
mtext(paste(" p-value = ",round(corFr_DL$p.value,3)))
dev.off()

#1.2 Flowerinf with Total Rainfall
Fl_DL = merge(dFl,DL,by="Date")
corFl_DL=cor.test(as.numeric(paste(Fl_DL$Value)),as.numeric(Fl_DL$Fl))

png("Graphs//Birds//CorrFl_DL.png",width=1080,height=720,units = "px", pointsize = 20)
plot(as.numeric(paste(Fl_DL$Value)),as.numeric(paste(Fl_DL$Fl)),ylab="% trees in flower",xlab="Daylength (hrs)",pch=20,col="navy")
abline(lm(as.numeric(paste(Fl_DL$Fl))~as.numeric(paste(Fl_DL$Value))), col="red")
mtext(paste(" p-value = ",round(corFl_DL$p.value,3)))
dev.off()


#Could you also do with Leaf flush (young leaves only)?
##Convert phenology to matrix to a dataframe with same key as weather data frame i.e. Month Year
len=nrow(YshP)*ncol(YshP)
dYsh = data.frame(Date=rep(NA, length=len),Rfr=rep(NA, length=len))

##Insert leaf flush percentages in new data frame
k=1
for(i in 1:nrow(YshP)){
  for(j in 1:ncol(YshP)){
    dYsh$Date[k]=paste(colnames(YshP)[j],rownames(YshP)[i])
    dYsh$Ysh[k]=YshP[i,j]*100
    k=k+1
  }
  
}
## 1.1 Leaf flush with Total Rainfall

Fr_TR = merge(dYsh,dTRain,by="Date")
corFr_TR=cor.test(as.numeric(paste(Fr_TR$TRain)),Fr_TR$Ysh)

png("Graphs//Birds//CorrYsh_TR.png",width=1080,height=720,units = "px", pointsize = 20)
plot(as.numeric(paste(Fr_TR$TRain)),Fr_TR$Ysh,ylab="% trees in leaf flush",xlab="Total monthly rainfall",pch=20,col="navy")
abline(lm(Fr_TR$Ysh~as.numeric(paste(Fr_TR$TRain))), col="red")
mtext(paste(" p-value = ",round(corFr_TR$p.value,3)))
dev.off()



## 2.1 Leaf flush with last 6 months Rainfall
corFr_TS=cor.test(as.numeric(paste(Fr_TR$TSRain)),Fr_TR$Ysh)

png("Graphs//Birds//CorrYsh_TS.png",width=1080,height=720,units = "px", pointsize = 20)
plot(as.numeric(paste(Fr_TR$TSRain)),as.numeric(paste(Fr_TR$Ysh)),ylab="% trees in leaf flush",xlab="Last 6 months rainfall",pch=20,col="navy")
abline(lm(as.numeric(paste(Fr_TR$Ysh))~as.numeric(paste(Fr_TR$TSRain))), col="red")
mtext(paste(" p-value = ",round(corFr_TS$p.value,3)))
dev.off()


## 3.1 Leaf flush with monthly min-max temperature

Fr_Tmin = merge(dYsh,dTMin,by="Date")
corFr_Tmin=cor.test(as.numeric(paste(Fr_Tmin$Value)),Fr_Tmin$Ysh)

png("Graphs//Birds//CorrYsh_Tmin.png",width=1080,height=720,units = "px", pointsize = 20)
plot(as.numeric(paste(Fr_Tmin$Value)),as.numeric(paste(Fr_Tmin$Ysh)),ylab="% trees in leaf flush",xlab="Minimum monthly temperature",pch=20,col="navy")
abline(lm(as.numeric(paste(Fr_Tmin$Ysh))~as.numeric(paste(Fr_Tmin$Value))), col="red")
mtext(paste(" p-value = ",round(corFr_Tmin$p.value,3)))
dev.off()

Fr_Tmax = merge(dYsh,dTMax,by="Date")
corFr_Tmax=cor.test(as.numeric(paste(Fr_Tmax$Value)),Fr_Tmax$Ysh)

png("Graphs//Birds//CorrYsh_Tmax.png",width=1080,height=720,units = "px", pointsize = 20)
plot(as.numeric(paste(Fr_Tmax$Value)),as.numeric(paste(Fr_Tmax$Ysh)),ylab="% trees in leaf flush",xlab="Maximum monthly temperature",pch=20,col="navy")
abline(lm(as.numeric(paste(Fr_Tmax$Ysh))~as.numeric(paste(Fr_Tmax$Value))), col="red")
mtext(paste(" p-value = ",round(corFr_Tmax$p.value,3)))
dev.off()


## 4.1 Leaf flush with Daylength

Fr_DL = merge(dYsh,DL,by="Date")
corFr_DL=cor.test(as.numeric(paste(Fr_DL$Value)),as.numeric(paste(Fr_DL$Ysh)))

png("Graphs//Birds//CorrYsh_DL.png",width=1080,height=720,units = "px", pointsize = 20)
plot(as.numeric(paste(Fr_DL$Value)),as.numeric(paste(Fr_DL$Ysh)),ylab="% trees in leaf flush",xlab="Daylength (hrs)",pch=20,col="navy")
abline(lm(as.numeric(paste(Fr_DL$Ysh))~as.numeric(paste(Fr_DL$Value))), col="red")
mtext(paste(" p-value = ",round(corFr_DL$p.value,3)))
dev.off()



