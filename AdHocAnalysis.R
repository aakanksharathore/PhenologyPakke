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
