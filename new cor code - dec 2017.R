setwd("C:/Users/Harman/Documents/aparajita.work")
dat<-read.csv(file="final.dat.csv", na.strings = c("NA",""))

install.packages("ggthemes")

library(dplyr)
library(tidyr)
library(psych)
library(GGally)
library(ggplot2)


species<-unique(dat$Scientific.name)
species <-species[!is.na(species)]
length(species) 

wea.dat<-read.csv(file="weather summary.csv", na.strings = c("NA",""))

wea.dat<-wea.dat[1:77,]
View(wea.dat)

?split
?corr.test

dat1<- dat %>% select(-c(1,2,3))
wea.dat1<- wea.dat %>% select(-c(1,2,3))
View(wea.dat1)

dat.species<-split(dat, dat$Scientific.name)
head(dat.species)
summary(dat.species)

merged.dat<-list()
for(i in 1:54)
{merged.dat[[i]]<-left_join(dat.species[[i]],wea.dat)}
View(merged.dat[[1]])

for(i in 1:54)
{merged.dat[[i]]<-merged.dat[[i]][-c(2,4,6),]}

df_FlB<- data.frame(label=c(1:54), est_max.rad=rep(NA, length=54), p_max.rad=rep(NA, length=54), est_mon.rain=rep(NA, length=54), p_mom.rain=rep(NA, length=54), est_temp=rep(NA, length=54), p_temp=rep(NA, length=54), est_min.temp=rep(NA, length=54), p_min.temp=rep(NA, length=54), est_max.temp=rep(NA, length=54), p_max.temp=rep(NA, length=54), est_RH=rep(NA, length=54), p_RH=rep(NA, length=54), est_wind=rep(NA, length=54), p_wind=rep(NA, length=54), est_gust=rep(NA, length=54), p_gust=rep(NA, length=54), est_rainy.days=rep(NA, length=54), p_rainy.days=rep(NA, length=54)) 

for(k in 1:54)
{j=1  
for (i in 1:9)
  {test<-cor.test(merged.dat[[k]]$percent_FlB, merged.dat[[k]][,i+10])
  df_FlB[k,j+1]= test$estimate
  df_FlB[k,j+2]= test$p.value
  j<-j+2}}
write.csv(df_FlB,file="FlB.weather.cor.csv")
View(df_FlB)

 
##

df_Fl<- data.frame(label=c(1:54), est_max.rad=rep(NA, length=54), p_max.rad=rep(NA, length=54), est_mon.rain=rep(NA, length=54), p_mom.rain=rep(NA, length=54), est_temp=rep(NA, length=54), p_temp=rep(NA, length=54), est_min.temp=rep(NA, length=54), p_min.temp=rep(NA, length=54), est_max.temp=rep(NA, length=54), p_max.temp=rep(NA, length=54), est_RH=rep(NA, length=54), p_RH=rep(NA, length=54), est_wind=rep(NA, length=54), p_wind=rep(NA, length=54), est_gust=rep(NA, length=54), p_gust=rep(NA, length=54), est_rainy.days=rep(NA, length=54), p_rainy.days=rep(NA, length=54)) 
for(k in 1:54)
{j=1
  for (i in 1:9)
{test<-cor.test(merged.dat[[k]]$percent_Fl, merged.dat[[k]][,i+10])
df_Fl[k,j+1]= test$estimate
df_Fl[k,j+2]= test$p.value
j<-j+2
}}
write.csv(df_Fl,file="Fl.weather.cor.csv")
View(df_Fl)

##

df_Rfr<- data.frame(label=c(1:54), est_max.rad=rep(NA, length=54), p_max.rad=rep(NA, length=54), est_mon.rain=rep(NA, length=54), p_mom.rain=rep(NA, length=54), est_temp=rep(NA, length=54), p_temp=rep(NA, length=54), est_min.temp=rep(NA, length=54), p_min.temp=rep(NA, length=54), est_max.temp=rep(NA, length=54), p_max.temp=rep(NA, length=54), est_RH=rep(NA, length=54), p_RH=rep(NA, length=54), est_wind=rep(NA, length=54), p_wind=rep(NA, length=54), est_gust=rep(NA, length=54), p_gust=rep(NA, length=54), est_rainy.days=rep(NA, length=54), p_rainy.days=rep(NA, length=54)) 
for(k in 1:54)
{j=1
  for (i in 1:9)
{test<-cor.test(merged.dat[[k]]$percent_Rfr, merged.dat[[k]][,i+10])
df_Rfr[k,j+1]= test$estimate
df_Rfr[k,j+2]= test$p.value
j<-j+2
}}
write.csv(df_Rfr,file="Rfr.weather.cor.csv")
View(df_Rfr)

##

df_Ysh<- data.frame(label=c(1:54), est_max.rad=rep(NA, length=54), p_max.rad=rep(NA, length=54), est_mon.rain=rep(NA, length=54), p_mom.rain=rep(NA, length=54), est_temp=rep(NA, length=54), p_temp=rep(NA, length=54), est_min.temp=rep(NA, length=54), p_min.temp=rep(NA, length=54), est_max.temp=rep(NA, length=54), p_max.temp=rep(NA, length=54), est_RH=rep(NA, length=54), p_RH=rep(NA, length=54), est_wind=rep(NA, length=54), p_wind=rep(NA, length=54), est_gust=rep(NA, length=54), p_gust=rep(NA, length=54), est_rainy.days=rep(NA, length=54), p_rainy.days=rep(NA, length=54)) 
for(k in 1:54)
{j=1
  for (i in 1:9)
{test<-cor.test(merged.dat[[k]]$percent_Ysh, merged.dat[[k]][,i+10])
df_Ysh[k,j+1]= test$estimate
df_Ysh[k,j+2]= test$p.value
j<-j+2
  }}
write.csv(df_Ysh,file="Ysh.weather.cor.csv")
View(df_Ysh)

####################
#scatter plots
######################


#ggpairs(merged.dat[[1]][,c(6,11:19)])
#ggpairs(merged.dat[[1]][,c(7,11:19)])
#ggpairs(merged.dat[[1]][,c(8,11:19)])
#ggpairs(merged.dat[[1]][,c(9,11:19)])

for(k in 1:54)
{for(i in 11:19)
  
{ #dev.copy(file="MyHist.png",device=png, bg="white",  width=640, height=352) 
  #graphics.off()
  a<-ggplot(merged.dat[[k]], aes(x=merged.dat[[k]][6], y=merged.dat[[k]][i])) +
    geom_point(shape=19) +
    geom_smooth(method=lm) +
    xlab("percent flower in bud") +
    ylab(colnames(merged.dat[[k]])[i]) +
    ggtitle("scatter plots with confidence intervals") +
    ggsave(paste('plot_', i, '.png', sep=''), a)
  print(a)
  dev.off
}
}
