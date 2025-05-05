#Script for presentation of results for the manuscript "Prioritising efficient use of resources: simple changes to monitoring combat inefficient capture-recapture of vulnerable skink"
#This script contains the data for Schnabel estimates and detection probabilities from CJS modelling performed in MARK

a<-read.csv("/WithinYearCSV.csv")

subN<-subset(a,a$N.R=="N")
table(subN$Year)

library('ggplot2')
library(tidyverse)
library(ggthemes)
library(multcompView)
library('fishmethods')
library('tibble')
library('ggpubr')

#Summary fo capture and recaptures per year
tabrawnum<-table(a$Year,a$N.R)
tabrawnum<-as.data.frame(tabrawnum)

table(a$Day,a$Year)

ggplot(tabrawnum, aes(fill=forcats::fct_rev(Var2), y=Freq, x=Var1)) + 
  geom_bar(position="stack", stat='identity', colour="black") +
  theme_classic() +
  ylab("Frequency")+
  xlab("Year") +
  labs(fill="N/R")+
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values=c("lightgrey","darkgrey"))+
  theme(axis.text.x = element_text(colour = "black",size = 14))+
  theme(axis.text.y = element_text( colour = "black",size = 14))+
  theme(axis.title = element_text( colour = "black",size = 14))+
  theme(legend.text = element_text( colour = "black",size = 14))+
  theme(legend.title = element_text( colour = "black",size = 14))


## Schnabel estimator method
#Reading in capture recapture values
#2017
a2017<-subset(a,a$Year==2017)
table(a2017$Day,a2017$N.R)

Ct<-c(16,0,16,8,0,0,0,2)
Rt<-c(0,0,1,0,0,0,0,0)
Mt<-c(16,0,15,8,0,0,0,2)

sch17<-schnabel(catch=Ct,recaps=Rt,newmarks=Mt)
sch17$Year<-2017
#2018
a2018<-subset(a,a$Year==2018)
table(a2018$Day,a2018$N.R)
Ct18<-c(17,16,4,12,8,10,16)
Rt18<-c(0,0,0,3,3,5,9)
Mt18<-c(17,16,4,9,5,5,7)

sch18<-schnabel(catch=Ct18,recaps=Rt18,newmarks=Mt18)
sch18$Year<-2018
#2019
a2019<-subset(a,a$Year==2019)
table(a2019$Day,a2019$N.R)
Ct19<-c(21,11,11,9,9,11,19)
Rt19<-c(0,2,2,2,4,2,5)
Mt19<-c(21,9,9,7,5,9,14)

sch19<-schnabel(catch=Ct19,recaps=Rt19,newmarks=Mt19)
sch19$Year<-2019
#2020
a2020<-subset(a,a$Year==2020)
table(a2020$Day,a2020$N.R)
Ct20<-c(27,14,13,33,14,18,17)
Rt20<-c(0,1,1,9,7,10,10)
Mt20<-c(27,13,12,24,7,8,7)

sch20<-schnabel(catch=Ct20,recaps=Rt20,newmarks=Mt20)
sch20$Year<-2020
#2021
a2021<-subset(a,a$Year==2021)
table(a2021$Day,a2021$N.R)
Ct21<-c(11,7,0,5,16,11,14)
Rt21<-c(0,0,0,0,1,2,3)
Mt21<-c(11,7,0,5,15,9,11)

sch21<-schnabel(catch=Ct21,recaps=Rt21,newmarks=Mt21)
sch21$Year<-2021
#2022
a2022<-subset(a,a$Year==2022)
table(a2022$Day,a2022$N.R)
Ct22<-c(14,3,6,3,1,8,9,5)
Rt22<-c(0,0,2,0,0,1,1,1)
Mt22<-c(14,3,4,3,1,7,8,4)

sch22<-schnabel(catch=Ct22,recaps=Rt22,newmarks=Mt22)
sch22$Year<-2022
#2023
a2023<-subset(a,a$Year==2023)
table(a2023$Day,a2023$N.R)
Ct23<-c(15,13,4,7,4,18)
Rt23<-c(0,1,2,2,3,2)
Mt23<-c(15,12,2,5,1,16)

sch23<-schnabel(catch=Ct23,recaps=Rt23,newmarks=Mt23)
sch23$Year<-2023
#2024
a2024<-subset(a,a$Year==2024)
table(a2024$Day,a2024$N.R)
Ct24<-c(10,9,12,9,15,12,9)
Rt24<-c(0,1,3,3,5,3,8)
Mt24<-c(10,8,9,6,10,9,1)

sch24<-schnabel(catch=Ct24,recaps=Rt24,newmarks=Mt24)
sch24$Year<-2024
#2025a - Estimate from the first 7 days only
a2025a<-subset(a,a$Year==2025 & as.numeric(a$Day) < 8)
table(a2025a$Day,a2025a$N.R)
Ct25a<-c(11,0,6,2,2,5,5)
Rt25a<-c(0,0,1,0,1,1,3)
Mt25a<-c(11,0,5,2,1,4,2)

sch25a<-schnabel(catch=Ct25a,recaps=Rt25a,newmarks=Mt25a)
sch25a$Year<-2025

#2025b - Estimate from all 14 days - only up to day 11 currently
a2025b<-subset(a,a$Year==2025)
table(a2025b$Day,a2025b$N.R)
Ct25b<-c(11,0,6,2,2,5,5,5,7,3,1,0,3,2)
Rt25b<-c(0,0,1,0,1,1,3,2,3,2,0,0,1,2)
Mt25b<-c(11,0,5,2,1,4,2,3,4,1,1,0,2,0)

sch25b<-schnabel(catch=Ct25b,recaps=Rt25b,newmarks=Mt25b)
sch25b$Year<-2025

schanbelAll<-rbind(sch17,sch18,sch19,sch20,sch21,sch22,sch23,sch24,sch25a,sch25b)

#Setting up df for ggplot2
df <- tibble::rownames_to_column(schanbelAll, "Estimator")

df$Estimator<-c("Schnabel","S-E","Schnabel","S-E","Schnabel","S-E","Schnabel","S-E","Schnabel","S-E","Schnabel","S-E","Schnabel","S-E","Schnabel","S-E","Schnabel","S-E","Schnabel (14 days)","S-E.b")

df$Estimator<-as.factor(df$Estimator)

#Replacing INf values with NA in df
df$LCI<-ifelse(df$LCI==-Inf,NA,df$LCI)
df$UCI<-ifelse(df$UCI==-Inf,NA,df$UCI)

df[1,4]<-NA
df[14,4]<-NA
df[14,5]<-NA
df<-df[3:20,]
df$Year<-as.factor(df$Year)

df<-subset(df,grepl("Schnabel",df$Estimator))
ggplot(df, aes(y=N, x=Year, colour = Estimator)) + 
  geom_point(position=position_dodge(width = .5), stat='identity',size=4.0) +
  geom_errorbar(aes(x = Year, ymin = LCI, ymax = UCI),data = df,position=position_dodge(width = .5),size=1.2)+
  theme_classic() +
  ylab("Population Estimate")+
  xlab("Year") +
  labs(fill="N/R")+
  scale_y_continuous(expand = c(0,0)) +
  scale_colour_manual(values=c("#238A8DFF","#55C667FF"))+
  theme(axis.text.x = element_text(colour = "black",size = 14))+
  theme(axis.text.y = element_text( colour = "black",size = 14))+
  theme(axis.title = element_text( colour = "black",size = 14))+
  theme(legend.text = element_text( colour = "black",size = 14))+
  theme(legend.title = element_text( colour = "black",size = 14))+
  coord_cartesian(ylim = c(0, 500)) +
  theme(legend.text = element_text( colour = "black",size = 14))+
  theme(legend.title = element_text( colour = "black",size = 14))+
  theme(legend.position =c(0.87,0.85))

ggplot(PopEstimatesAll, aes(y=N, x=Year, colour = Estimator)) + 
  geom_point(position=position_dodge(width = .5), stat='identity',size=4.0) +
  geom_errorbar(aes(x = Year, ymin = LCI, ymax = UCI),data = PopEstimatesAll,position=position_dodge(width = .5),size=1.2)+
  theme_classic() +
  ylab("Population Estimate")+
  xlab("Year") +
  labs(fill="Estimator")+
  scale_y_continuous(expand = c(0,0)) +
  scale_colour_manual(values=c("#238A8DFF","#55C667FF","#FDE725FF"))+
  theme(axis.text.x = element_text(colour = "black",size = 14))+
  theme(axis.text.y = element_text( colour = "black",size = 14))+
  theme(axis.title = element_text( colour = "black",size = 14))+
  theme(legend.text = element_text( colour = "black",size = 14))+
  theme(legend.title = element_text( colour = "black",size = 14))+
  coord_cartesian(ylim = c(0, 500))+
  theme(legend.position =c(0.87,0.85))

#------Population estimate CJS-------
#Calculating population estimates from CJS capture probabilities and s.e
nt<-c(41,48,70,80,28,39,40,48,36)
nt<-as.data.frame(nt)
nt$p<-c(0.1557376,0.5615120,0.4506234,0.4859787,0.2145728,0.4050199,0.1820820,0.6184842,0.3633857)
nt$se.p<-c(0.1478234,0.1122591,0.0942953,0.0870806,0.0604758,0.1039397,0.0732568,0.1370737,0.1087287)

Nhat_t=function(nt,pt){
  nt/pt
}
nt$popestimate<-Nhat_t(nt$nt, nt$p)

CINhathigh<-function(se,Nhat){
  CIhigh<-Nhat+(2*se)
    return(CIhigh)
}

CINhatlow<-function(se,Nhat){
  CIlow<-Nhat-(2*se)
  return(CIlow)
}

SENhat<-function(p,se.p,n){
  (n*se.p)/p^2
}

nt$se.nhat<-SENhat(nt$p,nt$se.p,nt$nt)

nt$NhatCIhigh<-CINhathigh(nt$se.nhat,nt$popestimate)

nt$NhatCIlow<-CINhatlow(nt$se.nhat,nt$popestimate)

nt$year<-c("2017","2018","2019","2020","2021","2022","2023","2024","2025")

ntno2017<-nt[-1,]

ggplot(ntno2017, aes(y=popestimate, x=year)) + 
  geom_point(stat='identity',size=4.0) +
  geom_errorbar(aes(x = year, ymin = NhatCIlow, ymax = NhatCIhigh),data = ntno2017,position=position_dodge(width = .5),size=1.2)+
  theme_classic() +
  ylab("Population Estimate")+
  xlab("Year") +
  scale_y_continuous(expand = c(0,0)) +
  scale_colour_manual(values=c("black","darkgrey"))+
  theme(axis.text.x = element_text(colour = "black",size = 14))+
  theme(axis.text.y = element_text( colour = "black",size = 14))+
  theme(axis.title = element_text( colour = "black",size = 14))+
  theme(legend.text = element_text( colour = "black",size = 14))+
  theme(legend.title = element_text( colour = "black",size = 14))+
  coord_cartesian(ylim = c(0, 500)) +
  theme(legend.position = "none")

#all together now
cjs.method<-as.data.frame(matrix(NA,8,7))
colnames(cjs.method)<-colnames(df)

cjs.method$Estimator <- "CJS"
cjs.method[,c(2,3,4,5,7)] <- ntno2017[,c(4,5,7,6,8)]

PopEstimatesAll <- rbind(df,cjs.method)


#re-order factor levels for region
PopEstimatesAll$Estimator <- factor(PopEstimatesAll$Estimator,levels=c('Schnabel', 'Schnabel.b', 'S-E','S-E.b', 'CJS'))
PopEstimatesAll$Year<-as.factor(PopEstimatesAll$Year)

ggplot(PopEstimatesAll, aes(y=N, x=Year, colour = Estimator)) + 
  geom_point(position=position_dodge(width = .5), stat='identity',size=4.0) +
  geom_errorbar(aes(x = Year, ymin = LCI, ymax = UCI),data = PopEstimatesAll,position=position_dodge(width = .5),size=1.2)+
  theme_classic() +
  ylab("Population Estimate")+
  xlab("Year") +
  labs(fill="Estimator")+
  scale_y_continuous(expand = c(0,0)) +
  scale_colour_manual(values=c("#238A8DFF","#55C667FF","#FDE725FF"))+
  theme(axis.text.x = element_text(colour = "black",size = 14))+
  theme(axis.text.y = element_text( colour = "black",size = 14))+
  theme(axis.title = element_text( colour = "black",size = 14))+
  theme(legend.text = element_text( colour = "black",size = 14))+
  theme(legend.title = element_text( colour = "black",size = 14))+
  coord_cartesian(ylim = c(0, 350))+
  theme(legend.position =c(0.87,0.85))

####----heterogeneity of capture probability--------

newdata<-aggregate(a$N.R,list(a$Day,a$Year),length)

newdata<-newdata[-which(newdata$Group.1==0),]
newdata[which(newdata$Group.1==1&newdata$Group.2==2025),3]=11
newdata[newdata$Group.1=="3A","Group.1"]=4

plot(newdata$Group.1,newdata$x)

newdata2<-aggregate(a$Date,list(a$Day,a$Year,a$N.R),length)
newdata2<-newdata2[-which(newdata2$Group.1==0),]
newdata2[which(newdata2$Group.1==1&newdata2$Group.2==2025),4]=11
newdata2[newdata2$Group.1=="3A","Group.1"]=4

newdata2$Group.1<-as.integer(newdata2$Group.1)
newdata2$Group.3<-as.factor(newdata2$Group.3)
plot(newdata2$x~newdata2$Group.1,col=newdata2$Group.3,pch = 19)

newdata2$Group.2<-as.factor(newdata2$Group.2)

newdata3<-subset(newdata2, newdata2$Group.1<8)
newdata3$Group.3<-as.factor(newdata3$Group.3)
newdata3$Group.1<-as.factor(newdata3$Group.1)

colnames(newdata3)<-colnames(newdata3, c("Group.1","Group.2","N/R","x"))

ggplot(newdata3, aes(y=x, x=Group.1, colour = Group.3)) + 
  geom_boxplot()+
  theme_classic() +
  ylab("")+
  xlab("Day of Survey") +
  scale_colour_manual(values=c("black","grey"))+
  scale_y_continuous(expand = c(0,0), limits = c(0,30)) +
  theme(axis.text.x = element_text(colour = "black",size = 14))+
  theme(axis.text.y = element_text( colour = "black",size = 14))+
  theme(axis.title = element_text( colour = "black",size = 14))+
  theme(legend.text = element_text( colour = "black",size = 14))+
  theme(legend.title = element_text( colour = "black",size = 14))+
  theme(legend.position = "none")
  
ggplot(newdata3, aes(y=x, x=Group.1)) + 
  geom_boxplot()+
  theme_classic() +
  ylab("Number of Captures")+
  xlab("Day of Survey") +
  scale_colour_manual(values=c("black","grey"))+
  scale_y_continuous(expand = c(0,0), limits = c(0,30)) +
  theme(axis.text.x = element_text(colour = "black",size = 14))+
  theme(axis.text.y = element_text( colour = "black",size = 14))+
  theme(axis.title = element_text( colour = "black",size = 14))+
  theme(legend.text = element_text( colour = "black",size = 14))+
  theme(legend.title = element_text( colour = "black",size = 14))