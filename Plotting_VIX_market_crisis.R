
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

#######################################################
### Pre-processing on Stat of one and two factor models
#######################################################

#get two factor stats
store_two_factor=result_two_factor

store_two_factor$a8c_runmed=runmed(as.double(result_two_factor$a8c),k=3,endrule = "median")

#this exact data frame stores the running median of a8c
runmed_two_factor=store_two_factor[,c(1,20)]

#calculate the running median of stat2 of two factor model
runmed_two_factor_stat2=as.data.table(stat2_final)

runmed_two_factor_stat2$stat2=as.double(runmed_two_factor_stat2$stat2)

runmed_two_factor_stat2 =  runmed_two_factor_stat2 %>% filter(stat2!='NA') 

runmed_two_factor_stat2$two_factor_stat2_runmed=runmed(as.double(runmed_two_factor_stat2$stat2),k=3,endrule='median')

two_factor_stats=merge(x=result_two_factor,y=runmed_two_factor,by='date')

two_factor_stats_2=merge(x=two_factor_stats,y=runmed_two_factor_stat2,by='date')

#subsetting all needed columns from the two_factor_stat2_2 data frame
two_factor_stats_subset=two_factor_stats_2[,c(1:5,20:22)]

names(two_factor_stats_subset)=c('date','two_factor_a5c','two_factor_a6c','two_factor_a7c','two_factor_a8c','two_factor_a8c_running_median','two_factor_stat2','two_factor_stat2_running_median')

#get one factor stats
one_factor_equity_subset=med_t[,c(1,2,3,11,13)]

names(one_factor_equity_subset)=c('date','one_factor_a3c','one_factor_a3c_running_median','one_factor_stat2','one_factor_stat2_running_median')

#merge one factor and two factor stats
dim(one_factor_equity_subset)
dim(two_factor_stats_subset)
one_factor_combine_two_factor_stat=merge(x=one_factor_equity_subset,y=two_factor_stats_subset,by='date')
dim(one_factor_combine_two_factor_stat)

#####################################################
### Identifying market crisis period by VIX index
#####################################################

### 1. Great Recession 2008-09-15 to 2009-06-30 # Demand Side Crisis
market_crisis_1 = subset(one_factor_combine_two_factor_stat,date >= '2008-09-15' & date <= '2009-06-30')

#the correlation of a3c in one factor model and a8c in two factor model (two intercept terms)
cor(market_crisis_1$one_factor_a3c,market_crisis_1$two_factor_a8c)
#result 0.6104966

#sign analysis for a3c and a8c
sign_analysis_crisis1=market_crisis_1[,c(1,2,9)]

sign_analysis_crisis1=cbind(sign_analysis_crisis1,ifelse(sign(sign_analysis_crisis1$one_factor_a3c)==sign(sign_analysis_crisis1$two_factor_a8c),1,0))

colnames(sign_analysis_crisis1)[4]<-'sign_a3c_and_a8c'
sign_perc_1=sum(sign_analysis_crisis1[,4])/nrow(sign_analysis_crisis1)
print(paste('percentage that a3c and a8c have the same sign is ',sign_perc_1))
#percentage is 0.82


### 2. Oil Spill in Gulf of Mexico 2010-04-20 to 2010-07-02 # Supply Side Crisis?

#VIX_oil_spill=subset(date_VIX_2008, Date>='2010-04-01')
#head(VIX_oil_spill,50)

market_crisis_2 = subset(one_factor_combine_two_factor_stat, date >= '2010-04-20' & date <= '2010-07-02')

#the correlation of a3c in one factor model and a8c in two factor model (two intercept terms)
cor(market_crisis_2$one_factor_a3c,market_crisis_2$two_factor_a8c)
#result 0.0903262

#sign analysis for a3c and a8c
sign_analysis_crisis2=market_crisis_2[,c(1,2,9)]

sign_analysis_crisis2=cbind(sign_analysis_crisis2,ifelse(sign(sign_analysis_crisis2$one_factor_a3c)==sign(sign_analysis_crisis2$two_factor_a8c),1,0))

colnames(sign_analysis_crisis2)[4]<-'sign_a3c_and_a8c'
sign_perc_2=sum(sign_analysis_crisis2[,4])/nrow(sign_analysis_crisis2)
print(paste('percentage that a3c and a8c have the same sign is ',sign_perc_2))
#percentage is 0.452830188679245


### 3. Arab Spring 

# VIX_arabspring=subset(date_VIX_2010, Date >= '2011-02-01')  #Demand Side Crisis?
# head(VIX_arabspring,70)



### 4. European Sovereign Debt Crisis 2011-08-02 to 2011-12-15

# VIX_euro=subset(date_VIX_2010, Date >= '2011-07-01')  #Demand Side Crisis?
# head(VIX_euro,70)

market_crisis_3 = subset(one_factor_combine_two_factor_stat, date >= '2011-08-02' & date <= '2011-12-15')

#the correlation of a3c in one factor model and a8c in two factor model (two intercept terms)
cor(market_crisis_3$one_factor_a3c,market_crisis_3$two_factor_a8c)
#result 0.5801527

#sign analysis for a3c and a8c
sign_analysis_crisis3=market_crisis_3[,c(1,2,9)]

sign_analysis_crisis3=cbind(sign_analysis_crisis3,ifelse(sign(sign_analysis_crisis3$one_factor_a3c)==sign(sign_analysis_crisis3$two_factor_a8c),1,0))

colnames(sign_analysis_crisis3)[4]<-'sign_a3c_and_a8c'
sign_perc_3=sum(sign_analysis_crisis3[,4])/nrow(sign_analysis_crisis3)
print(paste('percentage that a3c and a8c have the same sign is ',sign_perc_3))
#percentage is 0.854166666666667


### 5. Ebola Virus 2014-10-13 to 2014-10-16

# VIX_oil_crash=subset(date_VIX_2012, Date >= '2014-08-01')  #Demand Side Crisis?
# head(VIX_oil_crash,70)


### 6. Shanghai Composite Falls 2015-08-20 to 2015-10-08

market_crisis_4 = subset(one_factor_combine_two_factor_stat, date >= '2015-08-20' & date <= '2015-10-08')

#the correlation of a3c in one factor model and a8c in two factor model (two intercept terms)
cor(market_crisis_4$one_factor_a3c,market_crisis_4$two_factor_a8c)
#result 0.6664682

#sign analysis for a3c and a8c
sign_analysis_crisis4=market_crisis_4[,c(1,2,9)]

sign_analysis_crisis4=cbind(sign_analysis_crisis4,ifelse(sign(sign_analysis_crisis4$one_factor_a3c)==sign(sign_analysis_crisis4$two_factor_a8c),1,0))

colnames(sign_analysis_crisis4)[4]<-'sign_a3c_and_a8c'
sign_perc_4=sum(sign_analysis_crisis4[,4])/nrow(sign_analysis_crisis4)
print(paste('percentage that a3c and a8c have the same sign is ',sign_perc_4))
#percentage is 0.4


### 7. Stable Market - Remove all above dates

stable_market=one_factor_combine_two_factor_stat[!(one_factor_combine_two_factor_stat$date%in% market_crisis_1$date),]

stable_market=stable_market[!(stable_market$date%in% market_crisis_2),]

stable_market=stable_market[!(stable_market$date%in% market_crisis_3),]

stable_market=stable_market[!(stable_market$date%in% market_crisis_4),]

#the correlation of a3c in one factor model and a8c in two factor model (two intercept terms)
cor(stable_market$one_factor_a3c,stable_market$two_factor_a8c)
#result 0.5081306

#sign analysis for a3c and a8c
sign_analysis_stable=stable_market[,c(1,2,9)]

sign_analysis_stable=cbind(sign_analysis_stable,ifelse(sign(sign_analysis_stable$one_factor_a3c)==sign(sign_analysis_stable$two_factor_a8c),1,0))

colnames(sign_analysis_stable)[4]<-'sign_a3c_and_a8c'
sign_perc_s=sum(sign_analysis_stable[,4])/nrow(sign_analysis_stable)
print(paste('percentage that a3c and a8c have the same sign is ',sign_perc_s))
#percentage is 0.704147465437788


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



############################################################
#### Great Recession Statistics
############################################################

recession_stat=read.csv("C:/jiaocai/Other/Econometric Models/June1st_Meeting/Great_Recession_Stat.csv")

recession_stat$date=as.Date(recession_stat$date,"%m/%d/%Y")

#calculate from 04/30/2008 to 06/30/2009 Great Recession

Great_Recession_Comb=cbind(one_factor_combine_two_factor_stat[,c(1,2,3,9,10)],index_date_df)

delta_index=read.csv("C:/jiaocai/Other/Econometric Models/ICJComp_VIX_index.csv")

delta_index$DATE=as.Date(delta_index$DATE,"%m/%d/%Y")

#delta_index=delta_index[(delta_index$DATE %in% one_factor_combine_two_factor_stat$date),]

names(delta_index)<-c('date','VIX','ICJ','delta_VIX','delta_ICJ')

Great_Recession_Comb=cbind(Great_Recession_Comb,delta_index[,c(4,5)])

#Subset from 04/30/2008 to 06/30/2009

Great_Recession_all=subset(Great_Recession_Comb, date >='2008-04-30' & date <='2009-06-30')

Great_Recession_all=Great_Recession_all[,-6]

names(Great_Recession_all)=c('date','a3c','a3c_runmed','a8c','a8c_runmed','ICJ','VIX','delta_ICJ','delta_VIX')

#plot VIX and ICJ during the great recession period
png(file='VIX_and_ICJ_Great_Recession_All.png')
par(mfrow=c(2,1))

plot(Great_Recession_all$date,Great_Recession_all$VIX,type='l',col='purple',pch='o',main='VIX Over Date',xlab='Date',ylab='Index')

plot(Great_Recession_all$date,Great_Recession_all$ICJ,type='l',col='green',pch='*',main='ICJ Composite Over Date',xlab='Date',ylab='Index')

dev.off()

#plot a3c and a3c running median
png(file='One_factor_a3c_and_runmed_great_recession.png')
par(mfrow=c(2,1))

plot(Great_Recession_all$date,Great_Recession_all$a3c,type='l',col='purple',pch='o',main='One Factor alpha3 During Great Recession',xlab='Date',ylab='Value')

plot(Great_Recession_all$date,Great_Recession_all$a3c_runmed,type='l',col='green',pch='*',main='One Factor alpha3 runmed During Great Recession',xlab='Date',ylab='Value')

dev.off()

#plot a8c and a8c running median
png(file='Two_factor_a8c_and_runmed_great_recession.png')
par(mfrow=c(2,1))

plot(Great_Recession_all$date,Great_Recession_all$a8c,type='l',col='purple',pch='o',main='Two Factor alpha4 During Great Recession',xlab='Date',ylab='Value')

plot(Great_Recession_all$date,Great_Recession_all$a8c_runmed,type='l',col='green',pch='*',main='Two Factor alpha4 During Great Recession',xlab='Date',ylab='Value')

#plot the correlation for all variables
dev.off()
library(corrplot)

#storing the correlation for all the great recession time
cor_great_rec=cor(Great_Recession_all[,-1])

cor_great_rec

corrplot(cor_great_rec,method='number')
corrplot(cor_great_rec,method='ellipse')

#for all the time during the great recession
dev.off()

Great_Recession_0=subset(Great_Recession_all, date >='2008-09-15' & date <='2009-06-30')

Great_Recession_0=Great_Recession_0[,-c(6,8)]

#the correlation for great recession during the whole recession period
cor_great_rec_0=cor(Great_Recession_0[,-1])

corrplot(cor_great_rec_0,method='number')

cor_df_0=cbind(cor(Great_Recession_0$a3c,Great_Recession_0$a8c),cor(Great_Recession_0$a3c_runmed,Great_Recession_0$a8c_runmed),
	cor(Great_Recession_0$a3c,Great_Recession_0$VIX),cor(Great_Recession_0$a3c_runmed,Great_Recession_0$VIX),
	cor(Great_Recession_0$a3c,Great_Recession_0$delta_VIX),cor(Great_Recession_0$a3c_runmed,Great_Recession_0$delta_VIX),
	cor(Great_Recession_0$a8c,Great_Recession_0$VIX),cor(Great_Recession_0$a8c_runmed,Great_Recession_0$VIX),
	cor(Great_Recession_0$a8c,Great_Recession_0$delta_VIX),cor(Great_Recession_0$a8c_runmed,Great_Recession_0$delta_VIX))

cor_df_0

#divide the great recession time into several periods

###### 1. Period Entering Recession 09/15/2008 to 11/22/2008
dev.off()
Great_Recession_1=subset(Great_Recession_all, date >='2008-09-15' & date <='2008-11-22')

Great_Recession_1=Great_Recession_1[,-c(6,8)]

#the correlation for great recession during the 1st period
cor_great_rec_1=cor(Great_Recession_1[,-1])

corrplot(cor_great_rec_1,method='number')

cor_df_1=cbind(cor(Great_Recession_1$a3c,Great_Recession_1$a8c),cor(Great_Recession_1$a3c_runmed,Great_Recession_1$a8c_runmed),
	cor(Great_Recession_1$a3c,Great_Recession_1$VIX),cor(Great_Recession_1$a3c_runmed,Great_Recession_1$VIX),
	cor(Great_Recession_1$a3c,Great_Recession_1$delta_VIX),cor(Great_Recession_1$a3c_runmed,Great_Recession_1$delta_VIX),
	cor(Great_Recession_1$a8c,Great_Recession_1$VIX),cor(Great_Recession_1$a8c_runmed,Great_Recession_1$VIX),
	cor(Great_Recession_1$a8c,Great_Recession_1$delta_VIX),cor(Great_Recession_1$a8c_runmed,Great_Recession_1$delta_VIX))

cor_df_1

###### 2. Period great recession in the crisis 11/23/2008 to 03/08/2009
dev.off()
Great_Recession_2=subset(Great_Recession_all, date >='2008-11-23' & date <='2009-03-08')

Great_Recession_2=Great_Recession_2[,-c(6,8)]

#the correlation for great recession during the 2nd period
cor_great_rec_2=cor(Great_Recession_2[,-1])

corrplot(cor_great_rec_2,method='number')

cor_df_2=cbind(cor(Great_Recession_2$a3c,Great_Recession_2$a8c),cor(Great_Recession_2$a3c_runmed,Great_Recession_2$a8c_runmed),
	cor(Great_Recession_2$a3c,Great_Recession_2$VIX),cor(Great_Recession_2$a3c_runmed,Great_Recession_2$VIX),
	cor(Great_Recession_2$a3c,Great_Recession_2$delta_VIX),cor(Great_Recession_2$a3c_runmed,Great_Recession_2$delta_VIX),
	cor(Great_Recession_2$a8c,Great_Recession_2$VIX),cor(Great_Recession_2$a8c_runmed,Great_Recession_2$VIX),
	cor(Great_Recession_2$a8c,Great_Recession_2$delta_VIX),cor(Great_Recession_2$a8c_runmed,Great_Recession_2$delta_VIX))

cor_df_2

###### 3. Period Great Recession Exiting the crisis 03/09/2009 to 06/30/2009
dev.off()
Great_Recession_3=subset(Great_Recession_all, date >= '2009-03-09' & date <= '2009-06-30')

Great_Recession_3=Great_Recession_3[,-c(6,8)]

#the correlation for great recession during the 3rd period
cor_great_rec_3=cor(Great_Recession_3[,-1])

corrplot(cor_great_rec_3,method='number')

cor_df_3=cbind(cor(Great_Recession_3$a3c,Great_Recession_3$a8c),cor(Great_Recession_3$a3c_runmed,Great_Recession_3$a8c_runmed),
	cor(Great_Recession_3$a3c,Great_Recession_3$VIX),cor(Great_Recession_3$a3c_runmed,Great_Recession_3$VIX),
	cor(Great_Recession_3$a3c,Great_Recession_3$delta_VIX),cor(Great_Recession_3$a3c_runmed,Great_Recession_3$delta_VIX),
	cor(Great_Recession_3$a8c,Great_Recession_3$VIX),cor(Great_Recession_3$a8c_runmed,Great_Recession_3$VIX),
	cor(Great_Recession_3$a8c,Great_Recession_3$delta_VIX),cor(Great_Recession_3$a8c_runmed,Great_Recession_3$delta_VIX))

cor_df_3

# ##### 4. Period Great Recession Fed lowered its benchmark interest rate to zero 12/16/2008 to the day before President Obama signs into law 
# #a $787 billion stimulus package that includes tax cuts and money for infrastructure, schools, health care, and green energy 02/16/2009
# dev.off()
# Great_Recession_4=subset(Great_Recession_all, date >= '2008-12-16' & date <= '2009-02-16')

# #the correlation for great recession during the 4th period
# cor_great_rec_4=cor(Great_Recession_4[,-1])

# corrplot(cor_great_rec_4,method='number')

# cor_df_4=cbind(cor(Great_Recession_4$one_factor_a3c,Great_Recession_4$two_factor_a8c),cor(Great_Recession_4$one_factor_a3c,Great_Recession_4$ICJ),
# 	cor(Great_Recession_4$one_factor_a3c,Great_Recession_4$delta_ICJ),cor(Great_Recession_4$one_factor_a3c,Great_Recession_4$VIX),
# 	cor(Great_Recession_4$one_factor_a3c,Great_Recession_4$delta_VIX),cor(Great_Recession_4$two_factor_a8c,Great_Recession_4$ICJ),
# 	cor(Great_Recession_4$two_factor_a8c,Great_Recession_4$delta_ICJ),cor(Great_Recession_4$two_factor_a8c,Great_Recession_4$VIX),
# 	cor(Great_Recession_4$two_factor_a8c,Great_Recession_4$delta_VIX))

# cor_df_4

# ##### 5. Period Great Recession President Obama signs into stimulus package 02/17/2009 to End of Great Recession 06/30/2009
# dev.off()
# Great_Recession_5=subset(Great_Recession_all, date >= '2009-02-17' & date <= '2009-06-30')

# #the correlation for great recession during the 4th period
# cor_great_rec_5=cor(Great_Recession_5[,-1])

# corrplot(cor_great_rec_5,method='number')

# cor_df_5=cbind(cor(Great_Recession_5$one_factor_a3c,Great_Recession_5$two_factor_a8c),cor(Great_Recession_5$one_factor_a3c,Great_Recession_5$ICJ),
# 	cor(Great_Recession_5$one_factor_a3c,Great_Recession_5$delta_ICJ),cor(Great_Recession_5$one_factor_a3c,Great_Recession_5$VIX),
# 	cor(Great_Recession_5$one_factor_a3c,Great_Recession_5$delta_VIX),cor(Great_Recession_5$two_factor_a8c,Great_Recession_5$ICJ),
# 	cor(Great_Recession_5$two_factor_a8c,Great_Recession_5$delta_ICJ),cor(Great_Recession_5$two_factor_a8c,Great_Recession_5$VIX),
# 	cor(Great_Recession_5$two_factor_a8c,Great_Recession_5$delta_VIX))

# cor_df_5

#the dataframe of correlations during the great recession 
corr_df_all=rbind(cor_df_0,cor_df_1,cor_df_2,cor_df_3)
corr_df_all=as.data.frame(corr_df_all)
names(corr_df_all)=c('cor_a3c:a8c','cor_a3c_runmed:a8c_runmed','cor_a3c:VIX','cor_a3c_runmed:VIX','cor_a3c:delta_VIX',
	'cor_a3c_runmed:delta_VIX','cor_a8c:VIX','cor_a8c_runmed:VIX','cor_a8c:delta_VIX','cor_a8c_runmed:delta_VIX')
write.csv(x=corr_df_all,file='corr_output_v2.csv')

######

Great_Recession_sup=subset(Great_Recession_all, date >= '2008-08-01' & date <= '2009-06-30')

cor(Great_Recession_sup$VIX,Great_Recession_sup$ICJ)


###########################################################################
#June 8th One Factor Oil a1c and Two Factor a7c (terms for betas)
###########################################################################

#One_factor_oil model data stored in result_orig data.frame
OFO_stat=result_orig[,c(1,2,3,7,8,12,13)]

OFO_stat$a1c_runmed=runmed(as.double(OFO_stat$a1c),k=3,endrule = "median")

two_factor_result=result_two_factor[,c(1,2,3,4,5,12,13,14,15,20,21,22,23)]

two_factor_result$a5c_runmed=runmed(as.double(two_factor_result$a5c),k=3,endrule='median')

two_factor_result$a6c_runmed=runmed(as.double(two_factor_result$a6c),k=3,endrule='median')

two_factor_result$a7c_runmed=runmed(as.double(two_factor_result$a7c),k=3,endrule='median')

#one_factor_combine_two_factor_stat$a7c_runmed=runmed(as.double(one_factor_combine_two_factor_stat$two_factor_a7c),k=3,endrule='median')

OFO_twofactor_comb=merge(x=OFO_stat[,c(1,2,4,6,8)],y=two_factor_result[,c(1,2,3,4,6,7,8,10,11,12,14,15,16)],by='date')

#read csv from local drive
OVX_index=read.csv("C:/jiaocai/Other/Econometric Models/Ronn_ForwardBetas/Ronn_ForwardBetas/OVX_index.csv")

OVX_index$Date=as.Date(OVX_index$Date,"%m/%d/%Y")

colnames(OVX_index)[1]<-"date"

OFO_twofactor_comb=merge(x=OFO_twofactor_comb,y=OVX_index,by='date')

#plot OVX index data by periods

png('OVX_For_All_Time.png')
plot(OVX_index$date,OVX_index$OVX,type='l',col='blue',main='OVX Index Over Time',xlab='Date',ylab='Index')
dev.off()

#############################################
### The whole period correlation  2007-10-01 to 2018-04-27

OFO_twofactor_0=subset(OFO_twofactor_comb, date >= '2007-10-01' & date <= '2018-04-27')

corrplot(cor(OFO_twofactor_0[,-c(1,3,4,9,10,11,12,13,14)]),method='number')

corr_OFO_two_0=cbind(cor(OFO_twofactor_0$a1c,OFO_twofactor_0$a7c),cor(OFO_twofactor_0$a1c_runmed,OFO_twofactor_0$a7c_runmed),
	cor(OFO_twofactor_0$a1c,OFO_twofactor_0$OVX),cor(OFO_twofactor_0$a1c_runmed,OFO_twofactor_0$OVX),
	cor(OFO_twofactor_0$a1c,OFO_twofactor_0$delta_OVX),cor(OFO_twofactor_0$a1c_runmed,OFO_twofactor_0$delta_OVX),
	cor(OFO_twofactor_0$a7c,OFO_twofactor_0$OVX),cor(OFO_twofactor_0$a7c_runmed,OFO_twofactor_0$OVX),
	cor(OFO_twofactor_0$a7c,OFO_twofactor_0$delta_OVX),cor(OFO_twofactor_0$a7c_runmed,OFO_twofactor_0$delta_OVX),

	cor(OFO_twofactor_0$a5c,OFO_twofactor_0$OVX),cor(OFO_twofactor_0$a5c_runmed,OFO_twofactor_0$OVX),
	cor(OFO_twofactor_0$a5c,OFO_twofactor_0$delta_OVX),cor(OFO_twofactor_0$a5c_runmed,OFO_twofactor_0$delta_OVX),
	cor(OFO_twofactor_0$a6c,OFO_twofactor_0$OVX),cor(OFO_twofactor_0$a6c_runmed,OFO_twofactor_0$delta_OVX),
	cor(OFO_twofactor_0$a6c,OFO_twofactor_0$delta_OVX),cor(OFO_twofactor_0$a6c_runmed,OFO_twofactor_0$delta_OVX))

corr_OFO_two_0

# to see if in this part the alphas are statistically significant
# axc signif == 1 then it is statistically significant
perc_a1c_signifi_0=sum(OFO_twofactor_0$signifi_a1c)/nrow(OFO_twofactor_0)
print(paste("The percentage that a1c is statistically significant during period 0 is ",perc_a1c_signifi_0))

perc_a5c_signifi_0=sum(OFO_twofactor_0$signifi_a5c)/nrow(OFO_twofactor_0)
print(paste("The percentage that a5c is statistically significant during period 0 is ",perc_a5c_signifi_0))

perc_a6c_signifi_0=sum(OFO_twofactor_0$signifi_a6c)/nrow(OFO_twofactor_0)
print(paste("The percentage that a6c is statistically significant during period 0 is ",perc_a6c_signifi_0))

perc_a7c_signifi_0=sum(OFO_twofactor_0$signifi_a7c)/nrow(OFO_twofactor_0)
print(paste("The percentage that a7c is statistically significant during period 0 is ",perc_a7c_signifi_0))

# Add a column sign_a1c_a7c to analyze the fraction whether a1c and a7c have the same sign, 1 means the same and 0 means does not
OFO_twofactor_0$sign_a1c_a7c=ifelse(sign(OFO_twofactor_0$a1c)==sign(OFO_twofactor_0$a7c),1,0)

OFO_twofactor_0$sign_a1c_a7c_runmed=ifelse(sign(OFO_twofactor_0$a1c_runmed)==sign(OFO_twofactor_0$a7c_runmed),1,0)

perc_a1c_a7c_samesign_0=sum(OFO_twofactor_0$sign_a1c_a7c)/nrow(OFO_twofactor_0)
print(paste("The percentage that a1c and a7c have the same sign is ", perc_a1c_a7c_samesign_0))
perc_a1c_a7c_runmed_samesign_0=sum(OFO_twofactor_0$sign_a1c_a7c_runmed)/nrow(OFO_twofactor_0)
print(paste("The percentage that a1c_runmed and a7c_runmed have the same sign is", perc_a1c_a7c_runmed_samesign_0))

significance_sign_0=cbind(perc_a1c_signifi_0,perc_a5c_signifi_0,perc_a6c_signifi_0,perc_a7c_signifi_0,perc_a1c_a7c_samesign_0,perc_a1c_a7c_runmed_samesign_0)


###########################################
### First Period (Time Before Great Recession) 2007-10-01 to 2008-07-31

OFO_twofactor_1=subset(OFO_twofactor_comb, date >= '2007-10-01' & date <= '2008-07-31')

OVX_index_1=subset(OVX_index, date >= '2007-10-01' & date <= '2008-07-31')

#plot the OVX index during the 1st period
plot(OVX_index_1$date,OVX_index_1$OVX,type='l',col='green3',main='OVX Index during 1st period',xlab='Date',ylab='Index')

corrplot(cor(OFO_twofactor_1[,-c(1,3,4,9,10,11,12,13,14)]),method='number')

corr_OFO_two_1=cbind(cor(OFO_twofactor_1$a1c,OFO_twofactor_1$a7c),cor(OFO_twofactor_1$a1c_runmed,OFO_twofactor_1$a7c_runmed),
	cor(OFO_twofactor_1$a1c,OFO_twofactor_1$OVX),cor(OFO_twofactor_1$a1c_runmed,OFO_twofactor_1$OVX),
	cor(OFO_twofactor_1$a1c,OFO_twofactor_1$delta_OVX),cor(OFO_twofactor_1$a1c_runmed,OFO_twofactor_1$delta_OVX),
	cor(OFO_twofactor_1$a7c,OFO_twofactor_1$OVX),cor(OFO_twofactor_1$a7c_runmed,OFO_twofactor_1$OVX),
	cor(OFO_twofactor_1$a7c,OFO_twofactor_1$delta_OVX),cor(OFO_twofactor_1$a7c_runmed,OFO_twofactor_1$delta_OVX),

	cor(OFO_twofactor_1$a5c,OFO_twofactor_1$OVX),cor(OFO_twofactor_1$a5c_runmed,OFO_twofactor_1$OVX),
	cor(OFO_twofactor_1$a5c,OFO_twofactor_1$delta_OVX),cor(OFO_twofactor_1$a5c_runmed,OFO_twofactor_1$delta_OVX),
	cor(OFO_twofactor_1$a6c,OFO_twofactor_1$OVX),cor(OFO_twofactor_1$a6c_runmed,OFO_twofactor_1$delta_OVX),
	cor(OFO_twofactor_1$a6c,OFO_twofactor_1$delta_OVX),cor(OFO_twofactor_1$a6c_runmed,OFO_twofactor_1$delta_OVX))

corr_OFO_two_1

# to see if in this part the alphas are statistically significant
# axc signif == 1 then it is statistically significant
perc_a1c_signifi_1=sum(OFO_twofactor_1$signifi_a1c)/nrow(OFO_twofactor_1)
print(paste("The percentage that a1c is statistically significant during period 1 is ",perc_a1c_signifi_1))

perc_a5c_signifi_1=sum(OFO_twofactor_1$signifi_a5c)/nrow(OFO_twofactor_1)
print(paste("The percentage that a5c is statistically significant during period 1 is ",perc_a5c_signifi_1))

perc_a6c_signifi_1=sum(OFO_twofactor_1$signifi_a6c)/nrow(OFO_twofactor_1)
print(paste("The percentage that a6c is statistically significant during period 1 is ",perc_a6c_signifi_1))

perc_a7c_signifi_1=sum(OFO_twofactor_1$signifi_a7c)/nrow(OFO_twofactor_1)
print(paste("The percentage that a7c is statistically significant during period 1 is ",perc_a7c_signifi_1))

# Add a column sign_a1c_a7c to analyze the fraction whether a1c and a7c have the same sign, 1 means the same and 0 means does not
OFO_twofactor_1$sign_a1c_a7c=ifelse(sign(OFO_twofactor_1$a1c)==sign(OFO_twofactor_1$a7c),1,0)

OFO_twofactor_1$sign_a1c_a7c_runmed=ifelse(sign(OFO_twofactor_1$a1c_runmed)==sign(OFO_twofactor_1$a7c_runmed),1,0)

perc_a1c_a7c_samesign_1=sum(OFO_twofactor_1$sign_a1c_a7c)/nrow(OFO_twofactor_1)
print(paste("The percentage that a1c and a7c have the same sign is ", perc_a1c_a7c_samesign_1))
perc_a1c_a7c_runmed_samesign_1=sum(OFO_twofactor_1$sign_a1c_a7c_runmed)/nrow(OFO_twofactor_1)
print(paste("The percentage that a1c_runmed and a7c_runmed have the same sign is", perc_a1c_a7c_runmed_samesign_1))

significance_sign_1=cbind(perc_a1c_signifi_1,perc_a5c_signifi_1,perc_a6c_signifi_1,perc_a7c_signifi_1,perc_a1c_a7c_samesign_1,perc_a1c_a7c_runmed_samesign_1)
significance_sign_1

###########################################
### Second Period (Time in Great Recession) 2008-08-01 to 2009-02-28

OFO_twofactor_2=subset(OFO_twofactor_comb, date >= '2008-08-01' & date <= '2009-02-28')

#plot the OVX index during the 2nd period
plot(OFO_twofactor_2$date,OFO_twofactor_2$OVX,type='l',col='green',main='OVX Index during 2nd period',xlab='Date',ylab='Index')

corrplot(cor(OFO_twofactor_2[,-c(1,3,4,9,10,11,12,13,14)]),method='number')

corr_OFO_two_2=cbind(cor(OFO_twofactor_2$a1c,OFO_twofactor_2$a7c),cor(OFO_twofactor_2$a1c_runmed,OFO_twofactor_2$a7c_runmed),
	cor(OFO_twofactor_2$a1c,OFO_twofactor_2$OVX),cor(OFO_twofactor_2$a1c_runmed,OFO_twofactor_2$OVX),
	cor(OFO_twofactor_2$a1c,OFO_twofactor_2$delta_OVX),cor(OFO_twofactor_2$a1c_runmed,OFO_twofactor_2$delta_OVX),
	cor(OFO_twofactor_2$a7c,OFO_twofactor_2$OVX),cor(OFO_twofactor_2$a7c_runmed,OFO_twofactor_2$OVX),
	cor(OFO_twofactor_2$a7c,OFO_twofactor_2$delta_OVX),cor(OFO_twofactor_2$a7c_runmed,OFO_twofactor_2$delta_OVX),

	cor(OFO_twofactor_2$a5c,OFO_twofactor_2$OVX),cor(OFO_twofactor_2$a5c_runmed,OFO_twofactor_2$OVX),
	cor(OFO_twofactor_2$a5c,OFO_twofactor_2$delta_OVX),cor(OFO_twofactor_2$a5c_runmed,OFO_twofactor_2$delta_OVX),
	cor(OFO_twofactor_2$a6c,OFO_twofactor_2$OVX),cor(OFO_twofactor_2$a6c_runmed,OFO_twofactor_2$delta_OVX),
	cor(OFO_twofactor_2$a6c,OFO_twofactor_2$delta_OVX),cor(OFO_twofactor_2$a6c_runmed,OFO_twofactor_2$delta_OVX))

corr_OFO_two_2

# to see if in this part the alphas are statistically significant
# axc signif == 1 then it is statistically significant
perc_a1c_signifi_2=sum(OFO_twofactor_2$signifi_a1c)/nrow(OFO_twofactor_2)
print(paste("The percentage that a1c is statistically significant during period 2 is ",perc_a1c_signifi_2))

perc_a5c_signifi_2=sum(OFO_twofactor_2$signifi_a5c)/nrow(OFO_twofactor_2)
print(paste("The percentage that a5c is statistically significant during period 2 is ",perc_a5c_signifi_2))

perc_a6c_signifi_2=sum(OFO_twofactor_2$signifi_a6c)/nrow(OFO_twofactor_2)
print(paste("The percentage that a6c is statistically significant during period 2 is ",perc_a6c_signifi_2))

perc_a7c_signifi_2=sum(OFO_twofactor_2$signifi_a7c)/nrow(OFO_twofactor_2)
print(paste("The percentage that a7c is statistically significant during period 2 is ",perc_a7c_signifi_2))

# Add a column sign_a1c_a7c to analyze the fraction whether a1c and a7c have the same sign, 1 means the same and 0 means does not
OFO_twofactor_2$sign_a1c_a7c=ifelse(sign(OFO_twofactor_2$a1c)==sign(OFO_twofactor_2$a7c),1,0)

OFO_twofactor_2$sign_a1c_a7c_runmed=ifelse(sign(OFO_twofactor_2$a1c_runmed)==sign(OFO_twofactor_2$a7c_runmed),1,0)

perc_a1c_a7c_samesign_2=sum(OFO_twofactor_2$sign_a1c_a7c)/nrow(OFO_twofactor_2)
print(paste("The percentage that a1c and a7c have the same sign is ", perc_a1c_a7c_samesign_2))
perc_a1c_a7c_runmed_samesign_2=sum(OFO_twofactor_2$sign_a1c_a7c_runmed)/nrow(OFO_twofactor_2)
print(paste("The percentage that a1c_runmed and a7c_runmed have the same sign is", perc_a1c_a7c_runmed_samesign_2))

significance_sign_2=cbind(perc_a1c_signifi_2,perc_a5c_signifi_2,perc_a6c_signifi_2,perc_a7c_signifi_2,perc_a1c_a7c_samesign_2,perc_a1c_a7c_runmed_samesign_2)
significance_sign_2


###########################################
### Third Period (Time after great recession) 2009-03-01 to 2010-12-31

OFO_twofactor_3=subset(OFO_twofactor_comb, date >= '2009-03-01' & date <= '2010-12-31')

#plot the OVX index during the 3rd period
plot(OFO_twofactor_3$date,OFO_twofactor_3$OVX,type='l',col='green',main='OVX Index during 3rd period',xlab='Date',ylab='Index')

corrplot(cor(OFO_twofactor_3[,-c(1,3,4,9,10,11,12,13,14)]),method='number')

corr_OFO_two_3=cbind(cor(OFO_twofactor_3$a1c,OFO_twofactor_3$a7c),cor(OFO_twofactor_3$a1c_runmed,OFO_twofactor_3$a7c_runmed),
	cor(OFO_twofactor_3$a1c,OFO_twofactor_3$OVX),cor(OFO_twofactor_3$a1c_runmed,OFO_twofactor_3$OVX),
	cor(OFO_twofactor_3$a1c,OFO_twofactor_3$delta_OVX),cor(OFO_twofactor_3$a1c_runmed,OFO_twofactor_3$delta_OVX),
	cor(OFO_twofactor_3$a7c,OFO_twofactor_3$OVX),cor(OFO_twofactor_3$a7c_runmed,OFO_twofactor_3$OVX),
	cor(OFO_twofactor_3$a7c,OFO_twofactor_3$delta_OVX),cor(OFO_twofactor_3$a7c_runmed,OFO_twofactor_3$delta_OVX),

	cor(OFO_twofactor_3$a5c,OFO_twofactor_3$OVX),cor(OFO_twofactor_3$a5c_runmed,OFO_twofactor_3$OVX),
	cor(OFO_twofactor_3$a5c,OFO_twofactor_3$delta_OVX),cor(OFO_twofactor_3$a5c_runmed,OFO_twofactor_3$delta_OVX),
	cor(OFO_twofactor_3$a6c,OFO_twofactor_3$OVX),cor(OFO_twofactor_3$a6c_runmed,OFO_twofactor_3$delta_OVX),
	cor(OFO_twofactor_3$a6c,OFO_twofactor_3$delta_OVX),cor(OFO_twofactor_3$a6c_runmed,OFO_twofactor_3$delta_OVX))

corr_OFO_two_3

# to see if in this part the alphas are statistically significant
# axc signif == 1 then it is statistically significant
perc_a1c_signifi_3=sum(OFO_twofactor_3$signifi_a1c)/nrow(OFO_twofactor_3)
print(paste("The percentage that a1c is statistically significant during period 3 is ",perc_a1c_signifi_3))

perc_a5c_signifi_3=sum(OFO_twofactor_3$signifi_a5c)/nrow(OFO_twofactor_3)
print(paste("The percentage that a5c is statistically significant during period 3 is ",perc_a5c_signifi_3))

perc_a6c_signifi_3=sum(OFO_twofactor_3$signifi_a6c)/nrow(OFO_twofactor_3)
print(paste("The percentage that a6c is statistically significant during period 3 is ",perc_a6c_signifi_3))

perc_a7c_signifi_3=sum(OFO_twofactor_3$signifi_a7c)/nrow(OFO_twofactor_3)
print(paste("The percentage that a7c is statistically significant during period 3 is ",perc_a7c_signifi_3))

# Add a column sign_a1c_a7c to analyze the fraction whether a1c and a7c have the same sign, 1 means the same and 0 means does not
OFO_twofactor_3$sign_a1c_a7c=ifelse(sign(OFO_twofactor_3$a1c)==sign(OFO_twofactor_3$a7c),1,0)

OFO_twofactor_3$sign_a1c_a7c_runmed=ifelse(sign(OFO_twofactor_3$a1c_runmed)==sign(OFO_twofactor_3$a7c_runmed),1,0)

perc_a1c_a7c_samesign_3=sum(OFO_twofactor_3$sign_a1c_a7c)/nrow(OFO_twofactor_3)
print(paste("The percentage that a1c and a7c have the same sign is ", perc_a1c_a7c_samesign_3))
perc_a1c_a7c_runmed_samesign_3=sum(OFO_twofactor_3$sign_a1c_a7c_runmed)/nrow(OFO_twofactor_3)
print(paste("The percentage that a1c_runmed and a7c_runmed have the same sign is", perc_a1c_a7c_runmed_samesign_3))

significance_sign_3=cbind(perc_a1c_signifi_3,perc_a5c_signifi_3,perc_a6c_signifi_3,perc_a7c_signifi_3,perc_a1c_a7c_samesign_3,perc_a1c_a7c_runmed_samesign_3)
significance_sign_3

############################################
### Fourth Period (Arab Spring) 2011-01-01 to 2011-04-30

OFO_twofactor_4=subset(OFO_twofactor_comb, date >= '2011-01-01' & date <= '2011-04-30')

#plot the OVX index during the 4th period
plot(OFO_twofactor_4$date,OFO_twofactor_4$OVX,type='l',col='purple',main='OVX Index during 4th period',xlab='Date',ylab='Index')

corrplot(cor(OFO_twofactor_4[,-c(1,3,4,9,10,11,12,13,14)]),method='number')

corr_OFO_two_4=cbind(cor(OFO_twofactor_4$a1c,OFO_twofactor_4$a7c),cor(OFO_twofactor_4$a1c_runmed,OFO_twofactor_4$a7c_runmed),
	cor(OFO_twofactor_4$a1c,OFO_twofactor_4$OVX),cor(OFO_twofactor_4$a1c_runmed,OFO_twofactor_4$OVX),
	cor(OFO_twofactor_4$a1c,OFO_twofactor_4$delta_OVX),cor(OFO_twofactor_4$a1c_runmed,OFO_twofactor_4$delta_OVX),
	cor(OFO_twofactor_4$a7c,OFO_twofactor_4$OVX),cor(OFO_twofactor_4$a7c_runmed,OFO_twofactor_4$OVX),
	cor(OFO_twofactor_4$a7c,OFO_twofactor_4$delta_OVX),cor(OFO_twofactor_4$a7c_runmed,OFO_twofactor_4$delta_OVX),

	cor(OFO_twofactor_4$a5c,OFO_twofactor_4$OVX),cor(OFO_twofactor_4$a5c_runmed,OFO_twofactor_4$OVX),
	cor(OFO_twofactor_4$a5c,OFO_twofactor_4$delta_OVX),cor(OFO_twofactor_4$a5c_runmed,OFO_twofactor_4$delta_OVX),
	cor(OFO_twofactor_4$a6c,OFO_twofactor_4$OVX),cor(OFO_twofactor_4$a6c_runmed,OFO_twofactor_4$delta_OVX),
	cor(OFO_twofactor_4$a6c,OFO_twofactor_4$delta_OVX),cor(OFO_twofactor_4$a6c_runmed,OFO_twofactor_4$delta_OVX))

corr_OFO_two_4

# to see if in this part the alphas are statistically significant
# axc signif == 1 then it is statistically significant
perc_a1c_signifi_4=sum(OFO_twofactor_4$signifi_a1c)/nrow(OFO_twofactor_4)
print(paste("The percentage that a1c is statistically significant during period 4 is ",perc_a1c_signifi_4))

perc_a5c_signifi_4=sum(OFO_twofactor_4$signifi_a5c)/nrow(OFO_twofactor_4)
print(paste("The percentage that a5c is statistically significant during period 4 is ",perc_a5c_signifi_4))

perc_a6c_signifi_4=sum(OFO_twofactor_4$signifi_a6c)/nrow(OFO_twofactor_4)
print(paste("The percentage that a6c is statistically significant during period 4 is ",perc_a6c_signifi_4))

perc_a7c_signifi_4=sum(OFO_twofactor_4$signifi_a7c)/nrow(OFO_twofactor_4)
print(paste("The percentage that a7c is statistically significant during period 4 is ",perc_a7c_signifi_4))

# Add a column sign_a1c_a7c to analyze the fraction whether a1c and a7c have the same sign, 1 means the same and 0 means does not
OFO_twofactor_4$sign_a1c_a7c=ifelse(sign(OFO_twofactor_4$a1c)==sign(OFO_twofactor_4$a7c),1,0)

OFO_twofactor_4$sign_a1c_a7c_runmed=ifelse(sign(OFO_twofactor_4$a1c_runmed)==sign(OFO_twofactor_4$a7c_runmed),1,0)

perc_a1c_a7c_samesign_4=sum(OFO_twofactor_4$sign_a1c_a7c)/nrow(OFO_twofactor_4)
print(paste("The percentage that a1c and a7c have the same sign is ", perc_a1c_a7c_samesign_4))
perc_a1c_a7c_runmed_samesign_4=sum(OFO_twofactor_4$sign_a1c_a7c_runmed)/nrow(OFO_twofactor_4)
print(paste("The percentage that a1c_runmed and a7c_runmed have the same sign is", perc_a1c_a7c_runmed_samesign_4))

significance_sign_4=cbind(perc_a1c_signifi_4,perc_a5c_signifi_4,perc_a6c_signifi_4,perc_a7c_signifi_4,perc_a1c_a7c_samesign_4,perc_a1c_a7c_runmed_samesign_4)
significance_sign_4


############################################
### Fifth Period (From Arab Spring to Oil Crash) 2011-05-01 to 2014-06-19

OFO_twofactor_5=subset(OFO_twofactor_comb, date >= '2011-05-01' & date <= '2014-06-19')

#plot the OVX index during the 5th period
plot(OFO_twofactor_5$date,OFO_twofactor_5$OVX,type='l',col='brown',main='OVX Index during 5th period',xlab='Date',ylab='Index')

corrplot(cor(OFO_twofactor_5[,-c(1,3,4,9,10,11,12,13,14)]),method='number')

corr_OFO_two_5=cbind(cor(OFO_twofactor_5$a1c,OFO_twofactor_5$a7c),cor(OFO_twofactor_5$a1c_runmed,OFO_twofactor_5$a7c_runmed),
	cor(OFO_twofactor_5$a1c,OFO_twofactor_5$OVX),cor(OFO_twofactor_5$a1c_runmed,OFO_twofactor_5$OVX),
	cor(OFO_twofactor_5$a1c,OFO_twofactor_5$delta_OVX),cor(OFO_twofactor_5$a1c_runmed,OFO_twofactor_5$delta_OVX),
	cor(OFO_twofactor_5$a7c,OFO_twofactor_5$OVX),cor(OFO_twofactor_5$a7c_runmed,OFO_twofactor_5$OVX),
	cor(OFO_twofactor_5$a7c,OFO_twofactor_5$delta_OVX),cor(OFO_twofactor_5$a7c_runmed,OFO_twofactor_5$delta_OVX),

	cor(OFO_twofactor_5$a5c,OFO_twofactor_5$OVX),cor(OFO_twofactor_5$a5c_runmed,OFO_twofactor_5$OVX),
	cor(OFO_twofactor_5$a5c,OFO_twofactor_5$delta_OVX),cor(OFO_twofactor_5$a5c_runmed,OFO_twofactor_5$delta_OVX),
	cor(OFO_twofactor_5$a6c,OFO_twofactor_5$OVX),cor(OFO_twofactor_5$a6c_runmed,OFO_twofactor_5$delta_OVX),
	cor(OFO_twofactor_5$a6c,OFO_twofactor_5$delta_OVX),cor(OFO_twofactor_5$a6c_runmed,OFO_twofactor_5$delta_OVX))

corr_OFO_two_5

# to see if in this part the alphas are statistically significant
# axc signif == 1 then it is statistically significant
perc_a1c_signifi_5=sum(OFO_twofactor_5$signifi_a1c)/nrow(OFO_twofactor_5)
print(paste("The percentage that a1c is statistically significant during period 5 is ",perc_a1c_signifi_5))

perc_a5c_signifi_5=sum(OFO_twofactor_5$signifi_a5c)/nrow(OFO_twofactor_5)
print(paste("The percentage that a5c is statistically significant during period 5 is ",perc_a5c_signifi_5))

perc_a6c_signifi_5=sum(OFO_twofactor_5$signifi_a6c)/nrow(OFO_twofactor_5)
print(paste("The percentage that a6c is statistically significant during period 5 is ",perc_a6c_signifi_5))

perc_a7c_signifi_5=sum(OFO_twofactor_5$signifi_a7c)/nrow(OFO_twofactor_5)
print(paste("The percentage that a7c is statistically significant during period 5 is ",perc_a7c_signifi_5))

# Add a column sign_a1c_a7c to analyze the fraction whether a1c and a7c have the same sign, 1 means the same and 0 means does not
OFO_twofactor_5$sign_a1c_a7c=ifelse(sign(OFO_twofactor_5$a1c)==sign(OFO_twofactor_5$a7c),1,0)

OFO_twofactor_5$sign_a1c_a7c_runmed=ifelse(sign(OFO_twofactor_5$a1c_runmed)==sign(OFO_twofactor_5$a7c_runmed),1,0)

perc_a1c_a7c_samesign_5=sum(OFO_twofactor_5$sign_a1c_a7c)/nrow(OFO_twofactor_5)
print(paste("The percentage that a1c and a7c have the same sign is ", perc_a1c_a7c_samesign_5))
perc_a1c_a7c_runmed_samesign_5=sum(OFO_twofactor_5$sign_a1c_a7c_runmed)/nrow(OFO_twofactor_5)
print(paste("The percentage that a1c_runmed and a7c_runmed have the same sign is", perc_a1c_a7c_runmed_samesign_5))

significance_sign_5=cbind(perc_a1c_signifi_5,perc_a5c_signifi_5,perc_a6c_signifi_5,perc_a7c_signifi_5,perc_a1c_a7c_samesign_5,perc_a1c_a7c_runmed_samesign_5)
significance_sign_5

############################################
### Sixth Period (Oil Crash) 2014-06-20 to 2016-02-29

OFO_twofactor_6=subset(OFO_twofactor_comb, date >= '2014-06-20' & date <= '2016-02-29')

#plot the OVX index during the 6th period
plot(OFO_twofactor_6$date,OFO_twofactor_6$OVX,type='l',col='brown',main='OVX Index during 6th period',xlab='Date',ylab='Index')

corrplot(cor(OFO_twofactor_6[,-c(1,3,4,9,10,11,12,13,14)]),method='number')

corr_OFO_two_6=cbind(cor(OFO_twofactor_6$a1c,OFO_twofactor_6$a7c),cor(OFO_twofactor_6$a1c_runmed,OFO_twofactor_6$a7c_runmed),
	cor(OFO_twofactor_6$a1c,OFO_twofactor_6$OVX),cor(OFO_twofactor_6$a1c_runmed,OFO_twofactor_6$OVX),
	cor(OFO_twofactor_6$a1c,OFO_twofactor_6$delta_OVX),cor(OFO_twofactor_6$a1c_runmed,OFO_twofactor_6$delta_OVX),
	cor(OFO_twofactor_6$a7c,OFO_twofactor_6$OVX),cor(OFO_twofactor_6$a7c_runmed,OFO_twofactor_6$OVX),
	cor(OFO_twofactor_6$a7c,OFO_twofactor_6$delta_OVX),cor(OFO_twofactor_6$a7c_runmed,OFO_twofactor_6$delta_OVX),

	cor(OFO_twofactor_6$a5c,OFO_twofactor_6$OVX),cor(OFO_twofactor_6$a5c_runmed,OFO_twofactor_6$OVX),
	cor(OFO_twofactor_6$a5c,OFO_twofactor_6$delta_OVX),cor(OFO_twofactor_6$a5c_runmed,OFO_twofactor_6$delta_OVX),
	cor(OFO_twofactor_6$a6c,OFO_twofactor_6$OVX),cor(OFO_twofactor_6$a6c_runmed,OFO_twofactor_6$delta_OVX),
	cor(OFO_twofactor_6$a6c,OFO_twofactor_6$delta_OVX),cor(OFO_twofactor_6$a6c_runmed,OFO_twofactor_6$delta_OVX))

corr_OFO_two_6

# to see if in this part the alphas are statistically significant
# axc signif == 1 then it is statistically significant
perc_a1c_signifi_6=sum(OFO_twofactor_6$signifi_a1c)/nrow(OFO_twofactor_6)
print(paste("The percentage that a1c is statistically significant during period 6 is ",perc_a1c_signifi_6))

perc_a5c_signifi_6=sum(OFO_twofactor_6$signifi_a5c)/nrow(OFO_twofactor_6)
print(paste("The percentage that a5c is statistically significant during period 6 is ",perc_a5c_signifi_6))

perc_a6c_signifi_6=sum(OFO_twofactor_6$signifi_a6c)/nrow(OFO_twofactor_6)
print(paste("The percentage that a6c is statistically significant during period 6 is ",perc_a6c_signifi_6))

perc_a7c_signifi_6=sum(OFO_twofactor_6$signifi_a7c)/nrow(OFO_twofactor_6)
print(paste("The percentage that a7c is statistically significant during period 6 is ",perc_a7c_signifi_6))

# Add a column sign_a1c_a7c to analyze the fraction whether a1c and a7c have the same sign, 1 means the same and 0 means does not
OFO_twofactor_6$sign_a1c_a7c=ifelse(sign(OFO_twofactor_6$a1c)==sign(OFO_twofactor_6$a7c),1,0)

OFO_twofactor_6$sign_a1c_a7c_runmed=ifelse(sign(OFO_twofactor_6$a1c_runmed)==sign(OFO_twofactor_6$a7c_runmed),1,0)

perc_a1c_a7c_samesign_6=sum(OFO_twofactor_6$sign_a1c_a7c)/nrow(OFO_twofactor_6)
print(paste("The percentage that a1c and a7c have the same sign is ", perc_a1c_a7c_samesign_6))
perc_a1c_a7c_runmed_samesign_6=sum(OFO_twofactor_6$sign_a1c_a7c_runmed)/nrow(OFO_twofactor_6)
print(paste("The percentage that a1c_runmed and a7c_runmed have the same sign is", perc_a1c_a7c_runmed_samesign_6))

significance_sign_6=cbind(perc_a1c_signifi_6,perc_a5c_signifi_6,perc_a6c_signifi_6,perc_a7c_signifi_6,perc_a1c_a7c_samesign_6,perc_a1c_a7c_runmed_samesign_6)
significance_sign_6

############################################
### Seventh Period (After Oil Crash Till the Lastest Period) 2016-03-01 to 2017-06-30

OFO_twofactor_7=subset(OFO_twofactor_comb, date >= '2016-03-01')

#plot the OVX index during the 7th period
plot(OFO_twofactor_7$date,OFO_twofactor_7$OVX,type='l',col='darkorchid',main='OVX Index during 7th period',xlab='Date',ylab='Index')

corrplot(cor(OFO_twofactor_7[,-c(1,3,4,9,10,11,12,13,14)]),method='number')

corr_OFO_two_7=cbind(cor(OFO_twofactor_7$a1c,OFO_twofactor_7$a7c),cor(OFO_twofactor_7$a1c_runmed,OFO_twofactor_7$a7c_runmed),
	cor(OFO_twofactor_7$a1c,OFO_twofactor_7$OVX),cor(OFO_twofactor_7$a1c_runmed,OFO_twofactor_7$OVX),
	cor(OFO_twofactor_7$a1c,OFO_twofactor_7$delta_OVX),cor(OFO_twofactor_7$a1c_runmed,OFO_twofactor_7$delta_OVX),
	cor(OFO_twofactor_7$a7c,OFO_twofactor_7$OVX),cor(OFO_twofactor_7$a7c_runmed,OFO_twofactor_7$OVX),
	cor(OFO_twofactor_7$a7c,OFO_twofactor_7$delta_OVX),cor(OFO_twofactor_7$a7c_runmed,OFO_twofactor_7$delta_OVX),

	cor(OFO_twofactor_7$a5c,OFO_twofactor_7$OVX),cor(OFO_twofactor_7$a5c_runmed,OFO_twofactor_7$OVX),
	cor(OFO_twofactor_7$a5c,OFO_twofactor_7$delta_OVX),cor(OFO_twofactor_7$a5c_runmed,OFO_twofactor_7$delta_OVX),
	cor(OFO_twofactor_7$a6c,OFO_twofactor_7$OVX),cor(OFO_twofactor_7$a6c_runmed,OFO_twofactor_7$delta_OVX),
	cor(OFO_twofactor_7$a6c,OFO_twofactor_7$delta_OVX),cor(OFO_twofactor_7$a6c_runmed,OFO_twofactor_7$delta_OVX))

corr_OFO_two_7

# to see if in this part the alphas are statistically significant
# axc signif == 1 then it is statistically significant
perc_a1c_signifi_7=sum(OFO_twofactor_7$signifi_a1c)/nrow(OFO_twofactor_7)
print(paste("The percentage that a1c is statistically significant during period 7 is ",perc_a1c_signifi_7))

perc_a5c_signifi_7=sum(OFO_twofactor_7$signifi_a5c)/nrow(OFO_twofactor_7)
print(paste("The percentage that a5c is statistically significant during period 7 is ",perc_a5c_signifi_7))

perc_a6c_signifi_7=sum(OFO_twofactor_7$signifi_a6c)/nrow(OFO_twofactor_7)
print(paste("The percentage that a6c is statistically significant during period 7 is ",perc_a6c_signifi_7))

perc_a7c_signifi_7=sum(OFO_twofactor_7$signifi_a7c)/nrow(OFO_twofactor_7)
print(paste("The percentage that a7c is statistically significant during period 7 is ",perc_a7c_signifi_7))

# Add a column sign_a1c_a7c to analyze the fraction whether a1c and a7c have the same sign, 1 means the same and 0 means does not
OFO_twofactor_7$sign_a1c_a7c=ifelse(sign(OFO_twofactor_7$a1c)==sign(OFO_twofactor_7$a7c),1,0)

OFO_twofactor_7$sign_a1c_a7c_runmed=ifelse(sign(OFO_twofactor_7$a1c_runmed)==sign(OFO_twofactor_7$a7c_runmed),1,0)

perc_a1c_a7c_samesign_7=sum(OFO_twofactor_7$sign_a1c_a7c)/nrow(OFO_twofactor_7)
print(paste("The percentage that a1c and a7c have the same sign is ", perc_a1c_a7c_samesign_7))
perc_a1c_a7c_runmed_samesign_7=sum(OFO_twofactor_7$sign_a1c_a7c_runmed)/nrow(OFO_twofactor_7)
print(paste("The percentage that a1c_runmed and a7c_runmed have the same sign is", perc_a1c_a7c_runmed_samesign_7))

significance_sign_7=cbind(perc_a1c_signifi_7,perc_a5c_signifi_7,perc_a6c_signifi_7,perc_a7c_signifi_7,perc_a1c_a7c_samesign_7,perc_a1c_a7c_runmed_samesign_7)
significance_sign_7

#combine the statistics together
corr_OFO_two_all=rbind(corr_OFO_two_0,corr_OFO_two_1,corr_OFO_two_2,corr_OFO_two_3,corr_OFO_two_4,corr_OFO_two_5,corr_OFO_two_6,
	corr_OFO_two_7)
corr_OFO_two_all
corr_OFO_two_all=as.data.frame(corr_OFO_two_all)

names(corr_OFO_two_all)=c('cor_a1c:a7c','cor_a1c_runmed:a7c_runmed','cor_a1c:OVX','cor_a1c_runmed:OVX','cor_a1c:delta_OVX',
	'cor_a1c_runmed:delta_OVX','cor_a7c:OVX','cor_a7c_runmed:OVX','cor_a7c:delta_OVX','cor_a7c_runmed:delta_OVX',
	'cor_a5c:OVX','cor_a5c_runmed:OVX','cor_a5c:delta_OVX','cor_a5c_runmed:delta_OVX','cor_a6c:OVX','cor_a6c_runmed:OVX',
	'cor_a6c:delta_OVX','cor_a6c_runmed:delta_OVX')

signif_sign_all=rbind(significance_sign_0,significance_sign_1,significance_sign_2,significance_sign_3,significance_sign_4,
	significance_sign_5,significance_sign_6,significance_sign_7)
signif_sign_all=as.data.frame(signif_sign_all)
signif_sign_all

names(signif_sign_all)=c('percent_signif_a1c','percent_signif_a5c','percent_signif_a6c','percent_signif_a7c','percent_same_sign_a1c_a7c',
	'percent_same_sign_a1c_runmed_a7c_runmed')

#combine two data frames and then output files
OFO_two_output=cbind(corr_OFO_two_all,signif_sign_all)
write.csv(x=OFO_two_output,file='OFO_twofactor_output.csv')

#output the original stat for the models
#set column sign_a1c/sign_a7c to be a value that indicates whether a1c/a7c is positive. 1 means positive and -1 means negative.
OFO_twofactor_0$sign_a1c=ifelse(sign(OFO_twofactor_0$a1c)>0,1,-1)
OFO_twofactor_0$sign_a1c_runmed=ifelse(sign(OFO_twofactor_0$a1c_runmed)>0,1,-1)
OFO_twofactor_0$sign_a7c=ifelse(sign(OFO_twofactor_0$a7c)>0,1,-1)
OFO_twofactor_0$sign_a7c_runmed=ifelse(sign(OFO_twofactor_0$a7c_runmed)>0,1,-1)

write.csv(x=OFO_twofactor_0,file='OFO_two_allstat_1.csv')

################################################
### Plot all periods OVX to check 

pdf('Seven Periods of OVX.pdf')
par(mfrow=c(4,2))

plot(OVX_index_1$date,OVX_index_1$OVX,type='l',col='red',main='OVX Index 2007-10-01 to 2008-07-31',xlab='Date',ylab='Index')

plot(OFO_twofactor_2$date,OFO_twofactor_2$OVX,type='l',col='blue',main='OVX Index 2008-08-01 to 2009-02-28',xlab='Date',ylab='Index')

plot(OFO_twofactor_3$date,OFO_twofactor_3$OVX,type='l',col='orange',main='OVX Index 2009-03-01 to 2010-12-31',xlab='Date',ylab='Index')

plot(OFO_twofactor_4$date,OFO_twofactor_4$OVX,type='l',col='purple',main='OVX Index 2011-01-01 to 2011-04-30',xlab='Date',ylab='Index')

plot(OFO_twofactor_5$date,OFO_twofactor_5$OVX,type='l',col='green',main='OVX Index 2011-05-01 to 2014-06-19',xlab='Date',ylab='Index')

plot(OFO_twofactor_6$date,OFO_twofactor_6$OVX,type='l',col='brown',main='OVX Index 2014-06-20 to 2016-02-29',xlab='Date',ylab='Index')

plot(OFO_twofactor_7$date,OFO_twofactor_7$OVX,type='l',col='darkorchid',main='OVX Index 2016-03-01 to 2017-06-30',xlab='Date',ylab='Index')

dev.off()


######################################################################################################
###  June 14th Critical Days Analysis
######################################################################################################

# read SPX data from local drive
SPX_index=read.csv("C:/jiaocai/Other/Econometric Models/SPX_index.csv")

SPX_index=SPX_index[,c(1,5,8)]

SPX_index$Date=as.Date(SPX_index$Date,"%m/%d/%Y")

names(SPX_index)=c('date','SPX','Return_SPX')

png(file = "SPX_over_date.png")

plot(SPX_index$date,SPX_index$SPX,lty=1,type='l',col='green',pch='o',main='SPX over Date',xlab='date',ylab='index')

dev.off()

# combine all needed statistics to a data frame

index_all=merge(x=delta_index,y=SPX_index,by='date')

# one factor equity
result_equity_no_apc$a3c_runmed=runmed(as.double(result_equity_no_apc$a3c),k=3,endrule='median')

one_factor_equity_results=merge(x=result_equity_no_apc,y=stat2_final_one_factor,by='date')
one_factor_equity_results=merge(x=one_factor_equity_results,y=stat1_final_one_factor,by='date')

# two factor results

result_two_factor$a8c_runmed=runmed(as.double(result_two_factor$a8c),k=3,endrule='median')

two_factor_resultsss=merge(x=result_two_factor,y=stat2_final,by='date')
two_factor_resultsss=merge(x=two_factor_resultsss,y=stat1_final,by='date')

OFE_two_combine=merge(x=one_factor_equity_results[,c(1,2,3,4,5,20,21,22)],y=two_factor_resultsss[,c(1,2,3,4,5,20,21,22)],by='date')

OFE_two_index_combine=merge(x=OFE_two_combine,y=index_all,by='date')

write.csv(x=OFE_two_index_combine,file='OFE_two_index_combine.csv')

crit_day_OFE_two_factor=OFE_two_index_combine[,c(1,4,6,7,12,13,14,16,17,20,21)]

# The stat1 was not correct in previous output, replace it with the correct one
crit_day_OFE_two_factor=merge(x=crit_day_OFE_two_factor,y=all_stat_OFE[,c(1,3)],by='date')

crit_day_OFE_two_factor=crit_day_OFE_two_factor[,c(1,2,3,5,6,12,4,7,8,9,10,11)]

colnames(crit_day_OFE_two_factor)[7]='stat2_OFE'
colnames(crit_day_OFE_two_factor)[8]='stat2_two_factor'

#output file
write.csv(x=crit_day_OFE_two_factor,file='crit_day_OFE_two_factor.csv')

# June 15th calculation of correlation on a8c and Return_SPX
cal_neg=read.csv("C:/jiaocai/Other/Econometric Models/June 11th Meeting/cal_neg_corr.csv")

cor(cal_neg$a8c,cal_neg$Return_SPX)

####################################################################################
### Critical Days Analysis for One Factor Oil Model and Two Factor Model
####################################################################################

# One factor Oil
result_orig$a1c_runmed=runmed(as.double(result_orig$a1c),k=3,endrule='median')

#calculate stat2 for one factor oil (use a2c as the intercept term)
OFO_stats=merge(x=result_orig,y=merged_crude,by='date')
stat2_final_OFO = OFO_stats %>% filter(maturity==2) %>% group_by(date) %>% summarise(calc_stat2(sig,a2c,impvol)) 

stat1_

# Two Factor Model
result_two_factor$a7c_runmed=runmed(as.double(result_two_factor$a7c),k=3,endrule='median')


#######################################################################
###  June 17th
#######################################################################

#read sta files from local drive
#one factor equity
all_stat_OFE=read.dta13("C:/jiaocai/Other/Econometric Models/Jun_26th_Meeting/stat1 stata file/stata_code/Equity Beta/Data/alt_stats.dta")

#his_beta_all_stat=read.dta13("C:/jiaocai/Other/Econometric Models/Jun_26th_Meeting/stat1 stata file/stata_code/Equity Beta/Data/hist_beta_err_all.dta")

##########
merged_no_apc_m2=subset(merged_spx_no_apc,maturity==2)
write.csv(x=merged_no_apc_m2,file="merged_no_apc_m2.csv")


#########################################################################
# Critical Days Analysis for OFO and two factor model
#########################################################################

crit_day_OFO_two=merge(x=result_orig[,c(1,2,3,12)],y=result_two_factor[,c(1,2,3,4,5,21)],by='date')

#get return on CL. Use maturity==2, that is CL2
retCL_df=subset(nls_data,maturity == 2 & name == 'apa')
retCL_df=retCL_df[,c(1,7)]

#merge the statistics
crit_day_OFO_two=merge(x=crit_day_OFO_two,y=retCL_df,by='date')
crit_day_OFO_two=merge(x=crit_day_OFO_two,y=index_all[,c(1,2,3)],by='date')

#merge them with stat1 and stat2 of two models
crit_day_OFO_two=merge(x=crit_day_OFO_two,y=all_stat_OFE[,c(1,3)],by='date')

crit_day_OFO_two=merge(x=crit_day_OFO_two,y=stat2_final,by='date')

crit_day_OFO_two=merge(x=crit_day_OFO_two,y=stat2_final_OFO,by='date')

crit_day_OFO_two=crit_day_OFO_two[,c(1,2,4,7,9,13,15,14,11,12,10)]

colnames(crit_day_OFO_two)[8]='stat2_two_factor'

write.csv(x=crit_day_OFO_two,file='crit_day_OFO_two.csv')

crit_day_OFO_two=merge(x=crit_day_OFO_two,y=OVX_index[,c(1,2)],by='date')

write.csv(x=crit_day_OFO_two,file='crit_day_OFO_two_v2.csv')

######
OFE_two_index_combine=merge(x=OFE_two_index_combine,y=all_stat_OFE[,c(1,3)],by='date')

write.csv(x=OFE_two_index_combine,file='OFE_two_index_combine_v2.csv')

################################################################
################################# put all parameter together and their running medians together
##############################

#OFO
result_orig$a1c_runmed=runmed(as.double(result_orig$a1c),k=3,endrule="median")

result_orig$a2c_runmed=runmed(as.double(result_orig$a2c),k=3,endrule='median')

result_orig_out=result_orig[,c(1,2,3,12,13)]

write.csv(x=result_orig_out,file='One_Factor_Oil_param.csv')

#OFE
result_equity_no_apc$a1c_runmed=runmed(as.double(result_equity_no_apc$a1c),k=3,endrule="median")

result_equity_no_apc$a2c_runmed=runmed(as.double(result_equity_no_apc$a2c),k=3,endrule="median")

result_equity_no_apc$a3c_runmed=runmed(as.double(result_equity_no_apc$a3c),k=3,endrule="median")

result_equity_no_apc$a4c_runmed=runmed(as.double(result_equity_no_apc$a4c),k=3,endrule="median")

result_OFE_out=result_equity_no_apc[,c(1,2,3,4,5,21,20,22,23)]

write.csv(x=result_OFE_out,file='One_Factor_Equity_param.csv')

#Two Factor
result_two_factor$a5c_runmed=runmed(as.double(result_two_factor$a5c),k=3,endrule="median")

result_two_factor$a6c_runmed=runmed(as.double(result_two_factor$a6c),k=3,endrule="median")

result_two_factor$a7c_runmed=runmed(as.double(result_two_factor$a7c),k=3,endrule="median")

result_two_factor$a8c_runmed=runmed(as.double(result_two_factor$a8c),k=3,endrule="median")

result_two_factor_out=result_two_factor[,c(1,2,3,4,5,20,21,22,23)]

write.csv(x=result_two_factor_out,file='Two_Factor_param.csv')

#calculate correlation

cor_a3c_a8c=merge(x=result_OFE_out,y=result_two_factor_out,by='date')

cor(cor_a3c_a8c$a3c,cor_a3c_a8c$a8c)
#0.5299274

cor(cor_a3c_a8c$a3c_runmed,cor_a3c_a8c$a8c_runmed)
#0.53

cor_a3c_a8c=merge(x=cor_a3c_a8c,y=delta_index,by='date')

#calculate correlation a3c, a3c_runmed with VIX, delta_VIX
cor(cor_a3c_a8c$a3c,cor_a3c_a8c$VIX)
#-0.06620367

cor(cor_a3c_a8c$a3c_runmed,cor_a3c_a8c$VIX)
#-0.1038407

cor(cor_a3c_a8c$a8c,cor_a3c_a8c$VIX)
#-0.1249841

cor(cor_a3c_a8c$a8c_runmed,cor_a3c_a8c$VIX)
#-0.1425149

cor(cor_a3c_a8c$a3c,cor_a3c_a8c$delta_VIX)
#0.02067325

cor(cor_a3c_a8c$a3c_runmed,cor_a3c_a8c$delta_VIX)
#-0.006932619

cor(cor_a3c_a8c$a8c,cor_a3c_a8c$delta_VIX)
#1.770003e-05

cor(cor_a3c_a8c$a8c_runmed,cor_a3c_a8c$delta_VIX)
#-0.009415296


#################################### July 27th Addition of sub-period correlation between OFE a3c and two factor a8c
### First Period (Time Before Great Recession) 2007-10-01 to 2008-07-31

OFE_two_merged=merge(x=result_two_factor[,c(1,2,3,4,5,20,21,22,23)],y=result_equity_no_apc[,c(1,2,3,4,5,20,21,22,23)],by='date')

#redo the index_df procedure
names(index_df)=c('date','VIX','ICJ','delta_VIX','delta_ICJ')

OFE_two_merged=merge(x=OFE_two_merged,y=index_df,by='date')

OFE_two_merged_1=subset(OFE_two_merged, date >= '2007-10-01' & date <= '2008-07-31')

corr_OFE_two_1=cbind(cor(OFE_two_merged_1$a3c,OFE_two_merged_1$a8c),cor(OFE_two_merged_1$a3c_runmed,OFE_two_merged_1$a8c_runmed),
	cor(OFE_two_merged_1$a3c,OFE_two_merged_1$VIX),cor(OFE_two_merged_1$a3c_runmed,OFE_two_merged_1$VIX),
	cor(OFE_two_merged_1$a8c,OFE_two_merged_1$VIX),cor(OFE_two_merged_1$a8c_runmed,OFE_two_merged_1$VIX))

corr_OFE_two_1


### Second Period (After Great Recession) 2008-08-01 to 2009-02-28

OFE_two_merged_2=subset(OFE_two_merged, date >= '2008-08-01' & date <= '2009-02-28')

corr_OFE_two_2=cbind(cor(OFE_two_merged_2$a3c,OFE_two_merged_2$a8c),cor(OFE_two_merged_2$a3c_runmed,OFE_two_merged_2$a8c_runmed),
	cor(OFE_two_merged_2$a3c,OFE_two_merged_2$VIX),cor(OFE_two_merged_2$a3c_runmed,OFE_two_merged_2$VIX),
	cor(OFE_two_merged_2$a8c,OFE_two_merged_2$VIX),cor(OFE_two_merged_2$a8c_runmed,OFE_two_merged_2$VIX))

corr_OFE_two_2

### Third Period 2009-03-01 to 2010-12-31

OFE_two_merged_3=subset(OFE_two_merged, date >= '2009-03-01' & date <= '2010-12-31')

corr_OFE_two_3=cbind(cor(OFE_two_merged_3$a3c,OFE_two_merged_3$a8c),cor(OFE_two_merged_3$a3c_runmed,OFE_two_merged_3$a8c_runmed),
	cor(OFE_two_merged_3$a3c,OFE_two_merged_3$VIX),cor(OFE_two_merged_3$a3c_runmed,OFE_two_merged_3$VIX),
	cor(OFE_two_merged_3$a8c,OFE_two_merged_3$VIX),cor(OFE_two_merged_3$a8c_runmed,OFE_two_merged_3$VIX))

corr_OFE_two_3

### Fourth Period 2011-01-01 to 2011-04-30

OFE_two_merged_4=subset(OFE_two_merged, date >= '2011-01-01' & date <= '2011-04-30')

corr_OFE_two_4=cbind(cor(OFE_two_merged_4$a3c,OFE_two_merged_4$a8c),cor(OFE_two_merged_4$a3c_runmed,OFE_two_merged_4$a8c_runmed),
	cor(OFE_two_merged_4$a3c,OFE_two_merged_4$VIX),cor(OFE_two_merged_4$a3c_runmed,OFE_two_merged_4$VIX),
	cor(OFE_two_merged_4$a8c,OFE_two_merged_4$VIX),cor(OFE_two_merged_4$a8c_runmed,OFE_two_merged_4$VIX))

corr_OFE_two_4

### Fifth Period 2011-05-01 to 2014-06-19

OFE_two_merged_5=subset(OFE_two_merged, date >= '2011-05-01' & date <= '2014-06-19')

corr_OFE_two_5=cbind(cor(OFE_two_merged_5$a3c,OFE_two_merged_5$a8c),cor(OFE_two_merged_5$a3c_runmed,OFE_two_merged_5$a8c_runmed),
	cor(OFE_two_merged_5$a3c,OFE_two_merged_5$VIX),cor(OFE_two_merged_5$a3c_runmed,OFE_two_merged_5$VIX),
	cor(OFE_two_merged_5$a8c,OFE_two_merged_5$VIX),cor(OFE_two_merged_5$a8c_runmed,OFE_two_merged_5$VIX))

corr_OFE_two_5

### Sixth Period 2014-06-20 to 2016-02-29

OFE_two_merged_6=subset(OFE_two_merged, date >= '2014-06-20' & date <= '2016-02-29')

corr_OFE_two_6=cbind(cor(OFE_two_merged_6$a3c,OFE_two_merged_6$a8c),cor(OFE_two_merged_6$a3c_runmed,OFE_two_merged_6$a8c_runmed),
	cor(OFE_two_merged_6$a3c,OFE_two_merged_6$VIX),cor(OFE_two_merged_6$a3c_runmed,OFE_two_merged_6$VIX),
	cor(OFE_two_merged_6$a8c,OFE_two_merged_6$VIX),cor(OFE_two_merged_6$a8c_runmed,OFE_two_merged_6$VIX))

corr_OFE_two_6

### Seventh Period 2016-03-01 to 2017-06-30

OFE_two_merged_7=subset(OFE_two_merged, date >= '2016-03-01' & date <= '2017-06-30')

corr_OFE_two_7=cbind(cor(OFE_two_merged_7$a3c,OFE_two_merged_7$a8c),cor(OFE_two_merged_7$a3c_runmed,OFE_two_merged_7$a8c_runmed),
	cor(OFE_two_merged_7$a3c,OFE_two_merged_7$VIX),cor(OFE_two_merged_7$a3c_runmed,OFE_two_merged_7$VIX),
	cor(OFE_two_merged_7$a8c,OFE_two_merged_7$VIX),cor(OFE_two_merged_7$a8c_runmed,OFE_two_merged_7$VIX))

corr_OFE_two_7

# combine all dfs together
corr_OFE_two_df=rbind(corr_OFE_two_1,corr_OFE_two_2,corr_OFE_two_3,corr_OFE_two_4,corr_OFE_two_5,corr_OFE_two_6,corr_OFE_two_7)
corr_OFE_two_df=as.data.frame(corr_OFE_two_df)
names(corr_OFE_two_df)=c('cor_a3c_a8c','cor_a3c_runmed_a8c_runmed','cor_a3c_VIX','cor_a3c_runmed_VIX','cor_a8c_VIX','cor_a8c_runmed_VIX')

write.csv(x=corr_OFE_two_df,file='corr_OFE_two_df_subperiod.csv')


##################################
###  Sep 29th Sub Period correlation for One Factor Equity a1c and One Factor Oil a1c

OFE_OFO_merged=merge(x=result_equity_no_apc,y=result_orig,by='date')

OFE_OFO_merged=OFE_OFO_merged[,c(1,2,3,4,5,20,21,22,23,24,25,34,35)]

names(OFE_OFO_merged)=c('date','Equity_a1c','Equity_a2c','Equity_a3c','Equity_a4c','Equity_a1c_runmed','Equity_a2c_runmed','Equity_a3c_runmed',
	'Equity_a4c_runmed','Oil_a1c','Oil_a2c','Oil_a1c_runmed','Oil_a2c_runmed')

OFE_OFO_merged=as.data.frame(OFE_OFO_merged)

### First Period  2007-10-01 to 2008-07-31

OFE_OFO_merged_1=subset(OFE_OFO_merged, date >= '2007-10-01' & date <= '2008-07-31')

corr_OFE_OFO_1=cbind(cor(OFE_OFO_merged_1$Equity_a1c,OFE_OFO_merged_1$Oil_a1c),
	cor(OFE_OFO_merged_1$Equity_a1c_runmed,OFE_OFO_merged_1$Oil_a1c_runmed))

corr_OFE_OFO_1

### Second Period  2008-08-01 to 2009-02-28

OFE_OFO_merged_2=subset(OFE_OFO_merged, date >= '2008-08-01' & date <= '2009-02-28')

corr_OFE_OFO_2=cbind(cor(OFE_OFO_merged_2$Equity_a1c,OFE_OFO_merged_2$Oil_a1c),
	cor(OFE_OFO_merged_2$Equity_a1c_runmed,OFE_OFO_merged_2$Oil_a1c_runmed))

corr_OFE_OFO_2

### Third Period  2009-03-01 to 2010-12-31

OFE_OFO_merged_3=subset(OFE_OFO_merged, date >= '2009-03-01' & date <= '2010-12-31')

corr_OFE_OFO_3=cbind(cor(OFE_OFO_merged_3$Equity_a1c,OFE_OFO_merged_3$Oil_a1c),
	cor(OFE_OFO_merged_3$Equity_a1c_runmed,OFE_OFO_merged_3$Oil_a1c_runmed))

corr_OFE_OFO_3

### Fourth Period  2011-01-01 to 2011-04-30

OFE_OFO_merged_4=subset(OFE_OFO_merged, date >= '2011-01-01' & date <= '2011-04-30')

corr_OFE_OFO_4=cbind(cor(OFE_OFO_merged_4$Equity_a1c,OFE_OFO_merged_4$Oil_a1c),
	cor(OFE_OFO_merged_4$Equity_a1c_runmed,OFE_OFO_merged_4$Oil_a1c_runmed))

corr_OFE_OFO_4

### Fifth Period  2011-05-01 to 2014-06-19

OFE_OFO_merged_5=subset(OFE_OFO_merged, date >= '2011-05-01' & date <= '2014-06-19')

corr_OFE_OFO_5=cbind(cor(OFE_OFO_merged_5$Equity_a1c,OFE_OFO_merged_5$Oil_a1c),
	cor(OFE_OFO_merged_5$Equity_a1c_runmed,OFE_OFO_merged_5$Oil_a1c_runmed))

corr_OFE_OFO_5

### Sixth Period  2014-06-20 to 2016-02-29

OFE_OFO_merged_6=subset(OFE_OFO_merged, date >= '2014-06-20' & date <= '2016-02-29')

corr_OFE_OFO_6=cbind(cor(OFE_OFO_merged_6$Equity_a1c,OFE_OFO_merged_6$Oil_a1c),
	cor(OFE_OFO_merged_6$Equity_a1c_runmed,OFE_OFO_merged_6$Oil_a1c_runmed))

corr_OFE_OFO_6

### Seventh Period  2016-03-01 to 2017-06-30

OFE_OFO_merged_7=subset(OFE_OFO_merged, date >= '2016-03-01' & date <= '2017-06-30')

corr_OFE_OFO_7=cbind(cor(OFE_OFO_merged_7$Equity_a1c,OFE_OFO_merged_7$Oil_a1c),
	cor(OFE_OFO_merged_7$Equity_a1c_runmed,OFE_OFO_merged_7$Oil_a1c_runmed))

corr_OFE_OFO_7

corr_OFE_OFO_df=rbind(corr_OFE_OFO_1,corr_OFE_OFO_2,corr_OFE_OFO_3,corr_OFE_OFO_4,corr_OFE_OFO_5,corr_OFE_OFO_6,corr_OFE_OFO_7)
corr_OFE_OFO_df=as.data.frame(corr_OFE_OFO_df)
names(corr_OFE_OFO_df)=c('cor_OFE_a1c_OFO_a1c','cor_OFE_a1c_runmed_OFO_a1c_runmed')

write.csv(x=corr_OFE_OFO_df,file='corr_OFE_OFO_df_subperiod.csv')

cor(OFE_OFO_merged$Equity_a1c,OFE_OFO_merged$Oil_a1c)
# 0.02214402

cor(OFE_OFO_merged$Equity_a1c_runmed,OFE_OFO_merged$Oil_a1c_runmed)
# 0.01649016


##################################
###  Sep 29th Sub Period correlation for One Factor Equity a1c and Two factor a7c

#Period 1 to 7

corr_OFE_two_beta_1=cbind(cor(OFE_two_merged_1$a1c,OFE_two_merged_1$a7c),cor(OFE_two_merged_1$a1c_runmed,OFE_two_merged_1$a7c_runmed))

corr_OFE_two_beta_2=cbind(cor(OFE_two_merged_2$a1c,OFE_two_merged_2$a7c),cor(OFE_two_merged_2$a1c_runmed,OFE_two_merged_2$a7c_runmed))

corr_OFE_two_beta_3=cbind(cor(OFE_two_merged_3$a1c,OFE_two_merged_3$a7c),cor(OFE_two_merged_3$a1c_runmed,OFE_two_merged_3$a7c_runmed))

corr_OFE_two_beta_4=cbind(cor(OFE_two_merged_4$a1c,OFE_two_merged_4$a7c),cor(OFE_two_merged_4$a1c_runmed,OFE_two_merged_4$a7c_runmed))

corr_OFE_two_beta_5=cbind(cor(OFE_two_merged_5$a1c,OFE_two_merged_5$a7c),cor(OFE_two_merged_5$a1c_runmed,OFE_two_merged_5$a7c_runmed))

corr_OFE_two_beta_6=cbind(cor(OFE_two_merged_6$a1c,OFE_two_merged_6$a7c),cor(OFE_two_merged_6$a1c_runmed,OFE_two_merged_6$a7c_runmed))

corr_OFE_two_beta_7=cbind(cor(OFE_two_merged_7$a1c,OFE_two_merged_7$a7c),cor(OFE_two_merged_7$a1c_runmed,OFE_two_merged_7$a7c_runmed))

corr_beta_combine=rbind(corr_OFE_two_beta_1,corr_OFE_two_beta_2,corr_OFE_two_beta_3,corr_OFE_two_beta_4,corr_OFE_two_beta_5,
	corr_OFE_two_beta_6,corr_OFE_two_beta_7)

corr_beta_combine=as.data.frame(corr_beta_combine)
names(corr_beta_combine)=c('cor_a1c_a7c','cor_a1c_runmed_a7c_runmed')
write.csv(x=corr_beta_combine,file='corr_beta_combine_subperiod.csv')

cor(OFE_two_merged$a1c,OFE_two_merged$a7c)
# 0.08890986

cor(OFE_two_merged$a1c_runmed,OFE_two_merged$a7c_runmed)
# 0.07697341