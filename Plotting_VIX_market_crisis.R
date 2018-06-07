
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

Great_Recession_Comb=cbind(one_factor_combine_two_factor_stat[,c(1,2,9)],index_date_df)

delta_index=read.csv("C:/jiaocai/Other/Econometric Models/ICJComp_VIX_index.csv")

delta_index$DATE=as.Date(delta_index$DATE,"%m/%d/%Y")

delta_index=delta_index[(delta_index$DATE %in% one_factor_combine_two_factor_stat$date),]

names(delta_index)<-c('date','VIX','ICJ','delta_VIX','delta_ICJ')

Great_Recession_Comb=cbind(Great_Recession_Comb,delta_index[,c(4,5)])

#Subset from 04/30/2008 to 06/30/2009

Great_Recession_all=subset(Great_Recession_Comb, date >='2008-04-30' & date <='2009-06-30')

Great_Recession_all=Great_Recession_all[,-4]

#plot VIX and ICJ during the great recession period
par(mfrow=c(2,1))

plot(Great_Recession_all$date,Great_Recession_all$VIX,type='l',col='purple',pch='o',main='VIX Over Date',xlab='Date',ylab='Index')

plot(Great_Recession_all$date,Great_Recession_all$ICJ,type='l',col='green',pch='*',main='ICJ Composite Over Date',xlab='Date',ylab='Index')

#plot the correlation for all variables
library(corrplot)

#storing the correlation for all the great recession time
cor_great_rec=cor(Great_Recession_all[,-1])

cor_great_rec

corrplot(cor_great_rec,method='number')
corrplot(cor_great_rec,method='ellipse')

#for all the time during the great recession
dev.off()

Great_Recession_0=subset(Great_Recession_all, date >='2008-09-15' & date <='2009-06-30')

#the correlation for great recession during the 1st period
cor_great_rec_0=cor(Great_Recession_0[,-1])

corrplot(cor_great_rec_0,method='number')

cor_df_0=cbind(cor(Great_Recession_0$one_factor_a3c,Great_Recession_0$two_factor_a8c),cor(Great_Recession_0$one_factor_a3c,Great_Recession_0$ICJ),
	cor(Great_Recession_0$one_factor_a3c,Great_Recession_0$delta_ICJ),cor(Great_Recession_0$one_factor_a3c,Great_Recession_0$VIX),
	cor(Great_Recession_0$one_factor_a3c,Great_Recession_0$delta_VIX),cor(Great_Recession_0$two_factor_a8c,Great_Recession_0$ICJ),
	cor(Great_Recession_0$two_factor_a8c,Great_Recession_0$delta_ICJ),cor(Great_Recession_0$two_factor_a8c,Great_Recession_0$VIX),
	cor(Great_Recession_0$two_factor_a8c,Great_Recession_0$delta_VIX))

cor_df_0

#divide the great recession time into several periods

###### 1. Period before great recession 04/30/2008 to 09/14/2008
dev.off()
Great_Recession_1=subset(Great_Recession_all, date >='2008-04-30' & date <='2008-09-14')

#the correlation for great recession during the 1st period
cor_great_rec_1=cor(Great_Recession_1[,-1])

corrplot(cor_great_rec_1,method='number')

cor_df_1=cbind(cor(Great_Recession_1$one_factor_a3c,Great_Recession_1$two_factor_a8c),cor(Great_Recession_1$one_factor_a3c,Great_Recession_1$ICJ),
	cor(Great_Recession_1$one_factor_a3c,Great_Recession_1$delta_ICJ),cor(Great_Recession_1$one_factor_a3c,Great_Recession_1$VIX),
	cor(Great_Recession_1$one_factor_a3c,Great_Recession_1$delta_VIX),cor(Great_Recession_1$two_factor_a8c,Great_Recession_1$ICJ),
	cor(Great_Recession_1$two_factor_a8c,Great_Recession_1$delta_ICJ),cor(Great_Recession_1$two_factor_a8c,Great_Recession_1$VIX),
	cor(Great_Recession_1$two_factor_a8c,Great_Recession_1$delta_VIX))

cor_df_1

###### 2. Period Great Recession Bankruptcy Lehman Brothers 09/15/2008 to the day before US Treasury gives out the first TARP money 10/27/2008
dev.off()
Great_Recession_2=subset(Great_Recession_all, date >='2008-09-15' & date <='2008-10-27')

#the correlation for great recession during the 2nd period
cor_great_rec_2=cor(Great_Recession_2[,-1])

corrplot(cor_great_rec_2,method='number')

cor_df_2=cbind(cor(Great_Recession_2$one_factor_a3c,Great_Recession_2$two_factor_a8c),cor(Great_Recession_2$one_factor_a3c,Great_Recession_2$ICJ),
	cor(Great_Recession_2$one_factor_a3c,Great_Recession_2$delta_ICJ),cor(Great_Recession_2$one_factor_a3c,Great_Recession_2$VIX),
	cor(Great_Recession_2$one_factor_a3c,Great_Recession_2$delta_VIX),cor(Great_Recession_2$two_factor_a8c,Great_Recession_2$ICJ),
	cor(Great_Recession_2$two_factor_a8c,Great_Recession_2$delta_ICJ),cor(Great_Recession_2$two_factor_a8c,Great_Recession_2$VIX),
	cor(Great_Recession_2$two_factor_a8c,Great_Recession_2$delta_VIX))

cor_df_2

###### 3. Period Great Recession The US Treasury gives out the first TARP money 10/28/2008 to the day before Fed lowered its benchmark 
#interest rate to zero 12/15/2008
dev.off()
Great_Recession_3=subset(Great_Recession_all, date >= '2008-10-28' & date <= '2008-12-15')

#the correlation for great recession during the 3rd period
cor_great_rec_3=cor(Great_Recession_3[,-1])

corrplot(cor_great_rec_3,method='number')

cor_df_3=cbind(cor(Great_Recession_3$one_factor_a3c,Great_Recession_3$two_factor_a8c),cor(Great_Recession_3$one_factor_a3c,Great_Recession_3$ICJ),
	cor(Great_Recession_3$one_factor_a3c,Great_Recession_3$delta_ICJ),cor(Great_Recession_3$one_factor_a3c,Great_Recession_3$VIX),
	cor(Great_Recession_3$one_factor_a3c,Great_Recession_3$delta_VIX),cor(Great_Recession_3$two_factor_a8c,Great_Recession_3$ICJ),
	cor(Great_Recession_3$two_factor_a8c,Great_Recession_3$delta_ICJ),cor(Great_Recession_3$two_factor_a8c,Great_Recession_3$VIX),
	cor(Great_Recession_3$two_factor_a8c,Great_Recession_3$delta_VIX))

cor_df_3

##### 4. Period Great Recession Fed lowered its benchmark interest rate to zero 12/16/2008 to the day before President Obama signs into law 
#a $787 billion stimulus package that includes tax cuts and money for infrastructure, schools, health care, and green energy 02/16/2009
dev.off()
Great_Recession_4=subset(Great_Recession_all, date >= '2008-12-16' & date <= '2009-02-16')

#the correlation for great recession during the 4th period
cor_great_rec_4=cor(Great_Recession_4[,-1])

corrplot(cor_great_rec_4,method='number')

cor_df_4=cbind(cor(Great_Recession_4$one_factor_a3c,Great_Recession_4$two_factor_a8c),cor(Great_Recession_4$one_factor_a3c,Great_Recession_4$ICJ),
	cor(Great_Recession_4$one_factor_a3c,Great_Recession_4$delta_ICJ),cor(Great_Recession_4$one_factor_a3c,Great_Recession_4$VIX),
	cor(Great_Recession_4$one_factor_a3c,Great_Recession_4$delta_VIX),cor(Great_Recession_4$two_factor_a8c,Great_Recession_4$ICJ),
	cor(Great_Recession_4$two_factor_a8c,Great_Recession_4$delta_ICJ),cor(Great_Recession_4$two_factor_a8c,Great_Recession_4$VIX),
	cor(Great_Recession_4$two_factor_a8c,Great_Recession_4$delta_VIX))

cor_df_4

##### 5. Period Great Recession President Obama signs into stimulus package 02/17/2009 to End of Great Recession 06/30/2009
dev.off()
Great_Recession_5=subset(Great_Recession_all, date >= '2009-02-17' & date <= '2009-06-30')

#the correlation for great recession during the 4th period
cor_great_rec_5=cor(Great_Recession_5[,-1])

corrplot(cor_great_rec_5,method='number')

cor_df_5=cbind(cor(Great_Recession_5$one_factor_a3c,Great_Recession_5$two_factor_a8c),cor(Great_Recession_5$one_factor_a3c,Great_Recession_5$ICJ),
	cor(Great_Recession_5$one_factor_a3c,Great_Recession_5$delta_ICJ),cor(Great_Recession_5$one_factor_a3c,Great_Recession_5$VIX),
	cor(Great_Recession_5$one_factor_a3c,Great_Recession_5$delta_VIX),cor(Great_Recession_5$two_factor_a8c,Great_Recession_5$ICJ),
	cor(Great_Recession_5$two_factor_a8c,Great_Recession_5$delta_ICJ),cor(Great_Recession_5$two_factor_a8c,Great_Recession_5$VIX),
	cor(Great_Recession_5$two_factor_a8c,Great_Recession_5$delta_VIX))

cor_df_5

#the dataframe of correlations during the great recession 
corr_df_all=rbind(cor_df_0,cor_df_1,cor_df_2,cor_df_3,cor_df_4,cor_df_5)
corr_df_all=as.data.frame(corr_df_all)
names(corr_df_all)=c('cor_a3c_a8c','cor_a3c_ICJ','cor_a3c_delta_ICJ','cor_a3c_VIX','cor_a3c_delta_VIX','cor_a8c_ICJ','cor_a8c_delta_ICJ','cor_a8c_VIX','cor_a8c_delta_VIX')
write.csv(x=corr_df_all,file='corr_output.csv')