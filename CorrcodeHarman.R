#setwd("C:/Users/Harman/Documents/aparajita.work/new corr")

wea.dat<-read.csv(file="Apr2011-Nov2017Weatherdata(master)corrected.csv", na.strings = c("NA",""))
#wea.dat<-wea.dat[1:77,]
View(wea.dat)

library(dplyr)
library(tidyr)
library(psych)
library(ggplot2)

#################################################

dat<-read.csv(file="final.bird.csv", na.strings = c("NA",""))

merged.bird<-data.frame()
merged.bird<-left_join(dat,wea.dat)
View(merged.bird)

merged.bird<-merged.bird[-c(2,4,6),]

df_FlB<- data.frame(label=rep("FlB", 11),label=c("max.rad","mon.rain", "temp", "min.temp", "max.temp", "RH", "wind", "gust", "rainy.days","prev.three", "prev.six"), estimate=rep(NA,11), pvalue=rep(NA,11), rsquare=rep(NA,11)) 
for (i in 1:11)
{j=1
test<-cor.test(merged.bird$percent_FlB, merged.bird[,i+9])
df_FlB[i,j+2]= round(test$estimate,4)
df_FlB[i,j+3]= round(test$p.value,4)
df_FlB[i,j+4]= round(test$estimate*test$estimate,4)
}
View(df_FlB)

df_Fl<- data.frame(label=rep("Fl", 11),label=c("max.rad","mon.rain", "temp", "min.temp", "max.temp", "RH", "wind", "gust", "rainy.days","prev.three", "prev.six"), estimate=rep(NA,11), pvalue=rep(NA,11), rsquare=rep(NA,11)) 

for (i in 1:11)
{j=1
test<-cor.test(merged.bird$percent_Fl, merged.bird[,i+9])
df_Fl[i,j+2]= round(test$estimate,4)
df_Fl[i,j+3]= round(test$p.value,4)
df_Fl[i,j+4]= round(test$estimate*test$estimate,4)
}
View(df_Fl)

df_Rfr<- data.frame(label=rep("Rfr", 11),label=c("max.rad","mon.rain", "temp", "min.temp", "max.temp", "RH", "wind", "gust", "rainy.days","prev.three", "prev.six"), estimate=rep(NA,11), pvalue=rep(NA,11), rsquare=rep(NA,11)) 
for (i in 1:11)
{j=1
test<-cor.test(merged.bird$percent_Rfr, merged.bird[,i+9])
df_Rfr[i,j+2]= round(test$estimate,4)
df_Rfr[i,j+3]= round(test$p.value,4)
df_Rfr[i,j+4]= round(test$estimate*test$estimate,4)
}
View(df_Rfr)

df_Ysh<- data.frame(label=rep("Ysh", 11),label=c("max.rad","mon.rain", "temp", "min.temp", "max.temp", "RH", "wind", "gust", "rainy.days","prev.three", "prev.six"), estimate=rep(NA,11), pvalue=rep(NA,11), rsquare=rep(NA,11)) 
for (i in 1:11)
{j=1
test<-cor.test(merged.bird$percent_Ysh, merged.bird[,i+9])
df_Ysh[i,j+2]= round(test$estimate,4)
df_Ysh[i,j+3]= round(test$p.value,4)
df_Ysh[i,j+4]= round(test$estimate*test$estimate,4)
}
View(df_Ysh)

cor.bird<-rbind(df_FlB,df_Fl,df_Rfr,df_Ysh)
View(cor.bird)

write.csv(cor.bird,file="cummulative bird cor.csv")

View(merged.bird)

####################
#all scatter plots
######################

for (j in 1:4)
{for(i in 1:11)
{a<-ggplot(merged.bird, aes(x=merged.bird[,i+9], y=merged.bird[,j+4])) +
    geom_point(shape=19) +
    geom_smooth(method=lm, se=F) +
    ggtitle("Bird dispersed") +
    ylab(colnames(merged.bird[j+4]))+
    xlab(colnames(merged.bird[i+9]))+
    theme_bw()
    print(a)
#ggtitle("scatter plots for bird dispersed") +
ggsave(paste('cor_bird_', i, j, '.png', sep=''), a)
dev.off}}
colnames(merged.bird[5])

dat<-read.csv(file="final.wind.csv", na.strings = c("NA",""))

merged.wind<-data.frame()
merged.wind<-left_join(dat,wea.dat)
View(merged.wind)

merged.wind<-merged.wind[-c(2,4,6),]

df_FlB<- data.frame(label=rep("FlB", 11),label=c("max.rad","mon.rain", "temp", "min.temp", "max.temp", "RH", "wind", "gust", "rainy.days","prev.three", "prev.six"), estimate=rep(NA,11), pvalue=rep(NA,11), rsquare=rep(NA,11)) 
for (i in 1:11)
{j=1
test<-cor.test(merged.wind$percent_FlB, merged.wind[,i+9])
df_FlB[i,j+2]= round(test$estimate,4)
df_FlB[i,j+3]= round(test$p.value,4)
df_FlB[i,j+4]= round(test$estimate*test$estimate,4)
}
View(df_FlB)

df_Fl<- data.frame(label=rep("Fl", 11),label=c("max.rad","mon.rain", "temp", "min.temp", "max.temp", "RH", "wind", "gust", "rainy.days","prev.three", "prev.six"), estimate=rep(NA,11), pvalue=rep(NA,11), rsquare=rep(NA,11)) 

for (i in 1:11)
{j=1
test<-cor.test(merged.wind$percent_Fl, merged.wind[,i+9])
df_Fl[i,j+2]= round(test$estimate,4)
df_Fl[i,j+3]= round(test$p.value,4)
df_Fl[i,j+4]= round(test$estimate*test$estimate,4)
}
View(df_Fl)

df_Rfr<- data.frame(label=rep("Rfr", 11),label=c("max.rad","mon.rain", "temp", "min.temp", "max.temp", "RH", "wind", "gust", "rainy.days","prev.three", "prev.six"), estimate=rep(NA,11), pvalue=rep(NA,11), rsquare=rep(NA,11)) 
for (i in 1:11)
{j=1
test<-cor.test(merged.wind$percent_Rfr, merged.wind[,i+9])
df_Rfr[i,j+2]= round(test$estimate,4)
df_Rfr[i,j+3]= round(test$p.value,4)
df_Rfr[i,j+4]= round(test$estimate*test$estimate,4)
}
View(df_Rfr)

df_Ysh<- data.frame(label=rep("Ysh", 11),label=c("max.rad","mon.rain", "temp", "min.temp", "max.temp", "RH", "wind", "gust", "rainy.days","prev.three", "prev.six"), estimate=rep(NA,11), pvalue=rep(NA,11), rsquare=rep(NA,11)) 
for (i in 1:11)
{j=1
test<-cor.test(merged.wind$percent_Ysh, merged.wind[,i+9])
df_Ysh[i,j+2]= round(test$estimate,4)
df_Ysh[i,j+3]= round(test$p.value,4)
df_Ysh[i,j+4]= round(test$estimate*test$estimate,4)
}
View(df_Ysh)

cor.wind<-rbind(df_FlB,df_Fl,df_Rfr,df_Ysh)
View(cor.wind)

write.csv(cor.wind,file="cummulative wind cor.csv")



####################
#all scatter plots
######################

for (j in 1:4)
{for(i in 1:11)
{a<-ggplot(merged.wind, aes(x=merged.wind[,i+9], y=merged.wind[,j+4])) +
  geom_point(shape=19) +
  ggtitle("Wind dispersed")+
  geom_smooth(method=lm, se=F) +
  ylab(colnames(merged.wind[j+4])) +
  xlab(colnames(merged.wind[i+9]))
theme_bw()
print(a)
#ggtitle("scatter plots for bird dispersed") +
ggsave(paste('cor_wind_', i, j, '.png', sep=''), a)
dev.off}}


#########################################################
#########################################################

dat<-read.csv(file="final.animal.csv", na.strings = c("NA",""))

merged.animal<-data.frame()
merged.animal<-left_join(dat,wea.dat)
View(merged.animal)

merged.animal<-merged.animal[-c(2,4,6),]

df_FlB<- data.frame(label=rep("FlB", 11),label=c("max.rad","mon.rain", "temp", "min.temp", "max.temp", "RH", "wind", "gust", "rainy.days","prev.three", "prev.six"), estimate=rep(NA,11), pvalue=rep(NA,11), rsquare=rep(NA,11)) 
for (i in 1:11)
{j=1
test<-cor.test(merged.animal$percent_FlB, merged.animal[,i+9])
df_FlB[i,j+2]= round(test$estimate,4)
df_FlB[i,j+3]= round(test$p.value,4)
df_FlB[i,j+4]= round(test$estimate*test$estimate,4)
}
View(df_FlB)

df_Fl<- data.frame(label=rep("Fl", 11),label=c("max.rad","mon.rain", "temp", "min.temp", "max.temp", "RH", "wind", "gust", "rainy.days","prev.three", "prev.six"), estimate=rep(NA,11), pvalue=rep(NA,11), rsquare=rep(NA,11)) 

for (i in 1:11)
{j=1
test<-cor.test(merged.animal$percent_Fl, merged.animal[,i+9])
df_Fl[i,j+2]= round(test$estimate,4)
df_Fl[i,j+3]= round(test$p.value,4)
df_Fl[i,j+4]= round(test$estimate*test$estimate,4)
}
View(df_Fl)

df_Rfr<- data.frame(label=rep("Rfr", 11),label=c("max.rad","mon.rain", "temp", "min.temp", "max.temp", "RH", "wind", "gust", "rainy.days","prev.three", "prev.six"), estimate=rep(NA,11), pvalue=rep(NA,11), rsquare=rep(NA,11)) 
for (i in 1:11)
{j=1
test<-cor.test(merged.animal$percent_Rfr, merged.animal[,i+9])
df_Rfr[i,j+2]= round(test$estimate,4)
df_Rfr[i,j+3]= round(test$p.value,4)
df_Rfr[i,j+4]= round(test$estimate*test$estimate,4)
}
View(df_Rfr)

df_Ysh<- data.frame(label=rep("Ysh", 11),label=c("max.rad","mon.rain", "temp", "min.temp", "max.temp", "RH", "wind", "gust", "rainy.days","prev.three", "prev.six"), estimate=rep(NA,11), pvalue=rep(NA,11), rsquare=rep(NA,11)) 
for (i in 1:11)
{j=1
test<-cor.test(merged.animal$percent_Ysh, merged.animal[,i+9])
df_Ysh[i,j+2]= round(test$estimate,4)
df_Ysh[i,j+3]= round(test$p.value,4)
df_Ysh[i,j+4]= round(test$estimate*test$estimate,4)
}
View(df_Ysh)

cor.animal<-rbind(df_FlB,df_Fl,df_Rfr,df_Ysh)
View(cor.animal)

write.csv(cor.animal,file="cummulative animal cor.csv")



####################
#all scatter plots
######################

for (j in 1:4)
{for(i in 1:11)
{a<-ggplot(merged.animal, aes(x=merged.animal[,i+9], y=merged.animal[,j+4])) +
  geom_point(shape=19) +
  ggtitle("Animal dispersed")+
  geom_smooth(method=lm, se=F) +
  ylab(colnames(merged.animal[j+4])) +
  xlab(colnames(merged.animal[i+9]))+
    theme_bw()
  print(a)
  #ggtitle("scatter plots for bird dispersed") +
  ggsave(paste('cor_animal_', i, j, '.png', sep=''), a)
  dev.off}}

#################################################################
################################################################


dat<-read.csv(file="final.mammal.csv", na.strings = c("NA",""))

merged.mammal<-data.frame()
merged.mammal<-left_join(dat,wea.dat)
View(merged.mammal)

merged.mammal<-merged.mammal[-c(2,4,6),]

df_FlB<- data.frame(label=rep("FlB", 11),label=c("max.rad","mon.rain", "temp", "min.temp", "max.temp", "RH", "wind", "gust", "rainy.days","prev.three", "prev.six"), estimate=rep(NA,11), pvalue=rep(NA,11), rsquare=rep(NA,11)) 
for (i in 1:11)
{j=1
test<-cor.test(merged.mammal$percent_FlB, merged.mammal[,i+9])
df_FlB[i,j+2]= round(test$estimate,4)
df_FlB[i,j+3]= round(test$p.value,4)
df_FlB[i,j+4]= round(test$estimate*test$estimate,4)
}
View(df_FlB)

df_Fl<- data.frame(label=rep("Fl", 11),label=c("max.rad","mon.rain", "temp", "min.temp", "max.temp", "RH", "wind", "gust", "rainy.days","prev.three", "prev.six"), estimate=rep(NA,11), pvalue=rep(NA,11), rsquare=rep(NA,11)) 

for (i in 1:11)
{j=1
test<-cor.test(merged.mammal$percent_Fl, merged.mammal[,i+9])
df_Fl[i,j+2]= round(test$estimate,4)
df_Fl[i,j+3]= round(test$p.value,4)
df_Fl[i,j+4]= round(test$estimate*test$estimate,4)
}
View(df_Fl)

df_Rfr<- data.frame(label=rep("Rfr", 11),label=c("max.rad","mon.rain", "temp", "min.temp", "max.temp", "RH", "wind", "gust", "rainy.days","prev.three", "prev.six"), estimate=rep(NA,11), pvalue=rep(NA,11), rsquare=rep(NA,11)) 
for (i in 1:11)
{j=1
test<-cor.test(merged.mammal$percent_Rfr, merged.mammal[,i+9])
df_Rfr[i,j+2]= round(test$estimate,4)
df_Rfr[i,j+3]= round(test$p.value,4)
df_Rfr[i,j+4]= round(test$estimate*test$estimate,4)
}
View(df_Rfr)

df_Ysh<- data.frame(label=rep("Ysh", 11),label=c("max.rad","mon.rain", "temp", "min.temp", "max.temp", "RH", "wind", "gust", "rainy.days","prev.three", "prev.six"), estimate=rep(NA,11), pvalue=rep(NA,11), rsquare=rep(NA,11)) 
for (i in 1:11)
{j=1
test<-cor.test(merged.mammal$percent_Ysh, merged.mammal[,i+9])
df_Ysh[i,j+2]= round(test$estimate,4)
df_Ysh[i,j+3]= round(test$p.value,4)
df_Ysh[i,j+4]= round(test$estimate*test$estimate,4)
}
View(df_Ysh)

cor.mammal<-rbind(df_FlB,df_Fl,df_Rfr,df_Ysh)
View(cor.mammal)

write.csv(cor.mammal,file="cummulative mammal cor.csv")


####################
#all scatter plots
######################

for (j in 1:4)
{for(i in 1:11)
{a<-ggplot(merged.mammal, aes(x=merged.mammal[,i+9], y=merged.mammal[,j+4])) +
  geom_point(shape=19) +
  ggtitle("Mammal dispersed")+
  geom_smooth(method=lm, se=F) +
  ylab(colnames(merged.mammal[j+4])) +
  xlab(colnames(merged.mammal[i+9]))
theme_bw()
print(a)
#ggtitle("scatter plots for bird dispersed") +
ggsave(paste('cor_mammal_', i, j, '.png', sep=''), a)
dev.off}}

#########################################################
########################################################
dat<-read.csv(file="final.all.csv", na.strings = c("NA",""))

merged.all<-data.frame()
merged.all<-left_join(dat,wea.dat)
View(merged.all)

merged.all<-merged.all[-c(2,4,6),]

df_FlB<- data.frame(label=rep("FlB", 11),label=c("max.rad","mon.rain", "temp", "min.temp", "max.temp", "RH", "wind", "gust", "rainy.days","prev.three", "prev.six"), estimate=rep(NA,11), pvalue=rep(NA,11), rsquare=rep(NA,11)) 
for (i in 1:11)
{j=1
test<-cor.test(merged.all$percent_FlB, merged.all[,i+9])
df_FlB[i,j+2]= round(test$estimate,4)
df_FlB[i,j+3]= round(test$p.value,4)
df_FlB[i,j+4]= round(test$estimate*test$estimate,4)
}
View(df_FlB)

df_Fl<- data.frame(label=rep("Fl", 11),label=c("max.rad","mon.rain", "temp", "min.temp", "max.temp", "RH", "wind", "gust", "rainy.days","prev.three", "prev.six"), estimate=rep(NA,11), pvalue=rep(NA,11), rsquare=rep(NA,11)) 

for (i in 1:11)
{j=1
test<-cor.test(merged.all$percent_Fl, merged.all[,i+9])
df_Fl[i,j+2]= round(test$estimate,4)
df_Fl[i,j+3]= round(test$p.value,4)
df_Fl[i,j+4]= round(test$estimate*test$estimate,4)
}
View(df_Fl)

df_Rfr<- data.frame(label=rep("Rfr", 11),label=c("max.rad","mon.rain", "temp", "min.temp", "max.temp", "RH", "wind", "gust", "rainy.days","prev.three", "prev.six"), estimate=rep(NA,11), pvalue=rep(NA,11), rsquare=rep(NA,11)) 
for (i in 1:11)
{j=1
test<-cor.test(merged.all$percent_Rfr, merged.all[,i+9])
df_Rfr[i,j+2]= round(test$estimate,4)
df_Rfr[i,j+3]= round(test$p.value,4)
df_Rfr[i,j+4]= round(test$estimate*test$estimate,4)
}
View(df_Rfr)

df_Ysh<- data.frame(label=rep("Ysh", 11),label=c("max.rad","mon.rain", "temp", "min.temp", "max.temp", "RH", "wind", "gust", "rainy.days","prev.three", "prev.six"), estimate=rep(NA,11), pvalue=rep(NA,11), rsquare=rep(NA,11)) 
for (i in 1:11)
{j=1
test<-cor.test(merged.all$percent_Ysh, merged.all[,i+9])
df_Ysh[i,j+2]= round(test$estimate,4)
df_Ysh[i,j+3]= round(test$p.value,4)
df_Ysh[i,j+4]= round(test$estimate*test$estimate,4)
}
View(df_Ysh)

cor.all<-rbind(df_FlB,df_Fl,df_Rfr,df_Ysh)
View(cor.all)

write.csv(cor.all,file="cummulative all cor.csv")



####################
#all scatter plots
######################

for (j in 1:4)
{for(i in 1:11)
{a<-ggplot(merged.all, aes(x=merged.all[,i+9], y=merged.all[,j+4])) +
  geom_point(shape=19) +
  ggtitle("All species cummulative")+
  geom_smooth(method=lm, se=F) +
  ylab(colnames(merged.all[j+4])) +
  xlab(colnames(merged.all[i+9]))+
theme_bw()
print(a)
ggsave(paste('cor_all_species_', i, j, '.png', sep=''), a)
dev.off}}

