
##############################################
### Identify market crisis with VIX index
##############################################
library(readr)


date_VIX=read_csv('C:/jiaocai/Other/Econometric Models/VIX_index.csv')

date_VIX=as.data.frame(date_VIX)

date_VIX=date_VIX[(date_VIX$Date %in% result_two_factor$date),]

png(file = "VIX_over_date.png")

plot(date_VIX$Date,date_VIX$Open,lty=1,type='l',col='green',pch='o')
points(date_VIX$Date,date_VIX$Close,col='pink',pch='*')
lines(date_VIX$Date,date_VIX$Close,col='pink',type='l',lty=2)
legend(x= "topright", y=1.5,
  legend=c("VIX_Open","VIX_Close"), 
  col=c("green","pink"), pch=c("o","*"),lty=1:2,cex=0.8)

dev.off()

date_VIX$Date=as.Date(date_VIX$Date)

date_VIX_2008=subset(date_VIX, Date >= '2008-01-01' & Date <= '2010-12-31')

date_VIX_2010=subset(date_VIX, Date >= '2010-09-01' & Date <= '2012-12-31')

date_VIX_2012=subset(date_VIX, Date >= '2012-09-01' & Date <= '2014-12-31')

date_VIX_2014=subset(date_VIX, Date >= '2014-09-01' & Date <= '2017-12-31')

#plot 2008-2010
png(file = "VIX_2008~2010.png")

plot(date_VIX_2008$Date,date_VIX_2008$Open,lty=1,type='l',col='springgreen1',pch='o')
points(date_VIX_2008$Date,date_VIX_2008$Close,col='orchid1',pch='*')
lines(date_VIX_2008$Date,date_VIX_2008$Close,col='orchid1',type='l',lty=2)
legend(x= "topright", y=1.5,
  legend=c("VIX_Open","VIX_Close"), 
  col=c("springgreen1","orchid1"), pch=c("o","*"),lty=1:2,cex=0.8)

dev.off()

#plot 2010-2012
png(file = "VIX_2010~2012.png")

plot(date_VIX_2010$Date,date_VIX_2010$Open,lty=1,type='l',col='springgreen2',pch='o')
points(date_VIX_2010$Date,date_VIX_2010$Close,col='orchid2',pch='*')
lines(date_VIX_2010$Date,date_VIX_2010$Close,col='orchid2',type='l',lty=2)
legend(x= "topright", y=1.5,
  legend=c("VIX_Open","VIX_Close"), 
  col=c("springgreen2","orchid2"), pch=c("o","*"),lty=1:2,cex=0.8)

dev.off()

#plot 2012-2014
png(file = "VIX_2012~2014.png")

plot(date_VIX_2012$Date,date_VIX_2012$Open,lty=1,type='l',col='springgreen3',pch='o')
points(date_VIX_2012$Date,date_VIX_2012$Close,col='orchid3',pch='*')
lines(date_VIX_2012$Date,date_VIX_2012$Close,col='orchid3',type='l',lty=2)
legend(x= "topright", y=1.5,
  legend=c("VIX_Open","VIX_Close"), 
  col=c("springgreen3","orchid3"), pch=c("o","*"),lty=1:2,cex=0.8)

dev.off()

#plot 2014-2017
png(file = "VIX_2014~2017.png")

plot(date_VIX_2014$Date,date_VIX_2014$Open,lty=1,type='l',col='springgreen2',pch='o')
points(date_VIX_2014$Date,date_VIX_2014$Close,col='orchid4',pch='*')
lines(date_VIX_2014$Date,date_VIX_2014$Close,col='orchid4',type='l',lty=2)
legend(x= "topright", y=1.5,
  legend=c("VIX_Open","VIX_Close"), 
  col=c("springgreen4","orchid4"), pch=c("o","*"),lty=1:2,cex=0.8)

dev.off()


#############################################
### June 1st Modification
#############################################

library(readr)
#read VIX and ICJ index from file
index_df=read_csv("C:/jiaocai/Other/Econometric Models/ICJComp_VIX_index.csv")
index_df=as.data.frame(index_df)
index_df$DATE=as.Date(index_df$DATE,"%m/%d/%Y")

index_date_df=index_df[(index_df$DATE %in% one_factor_combine_two_factor_stat$date),]

names(index_date_df)=c('date','ICJ','VIX')

#plotting VIX close and ICJ close in the same plot

png(file = "VIX_and_ICJ_over_date.png")

plot(index_date_df$date,index_date_df$VIX,lty=1,type='l',col='purple',pch='o',main='VIX and ICJ Index Over Date',xlab='Date',ylab='Index')
points(index_date_df$date,index_date_df$ICJ,col='green',pch='*')
lines(index_date_df$date,index_date_df$ICJ,col='green',type='l',lty=2)
legend(x= "topright", y=1.5,
  legend=c("VIX","ICJ Composite"), 
  col=c("purple","green"), pch=c("o","*"),lty=1:2,cex=0.8)

dev.off()

#plotting VIX and ICJ seperately and put them in same window

png(file = "VIX_and_ICJ_over_date_sep.png")

par(mfrow=c(2,1))

plot(index_date_df$date,index_date_df$VIX,type='l',col='purple',pch='o',main='VIX Over Date',xlab='Date',ylab='Index')

plot(index_date_df$date,index_date_df$ICJ,type='l',col='green',pch='*',main='ICJ Composite Over Date',xlab='Date',ylab='Index')

dev.off()