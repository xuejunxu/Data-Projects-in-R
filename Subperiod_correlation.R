#################################### July 27th Addition of sub-period correlation between OFE a3c and two factor a8c
### First Period (Time Before Great Recession) 2007-10-01 to 2008-07-31


result_two_factor=read.csv('C:/jiaocai/Other/FinModelOutput/TWO_param.csv')

result_two_factor$date=as.Date(result_two_factor$date)

#result_equity_no_apc$a4c_runmed=runmed(as.double(result_equity_no_apc$a4c),k=3,endrule = "median")

result_equity_no_apc=read.csv('C:/jiaocai/Other/FinModelOutput/OFE_param.csv')

result_equity_no_apc$date=as.Date(result_equity_no_apc$date)

OFE_two_merged=merge(x=result_two_factor[,c(1,2,3,4,5,20,21,22,23)],y=result_equity_no_apc[,c(1,2,3,4,5,20,21,22,23)],by='date')

index_df=read.csv("C:/jiaocai/Other/Econometric Models/ICJComp_VIX_index.csv")

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

#result_orig$a2c_runmed=runmed(as.double(result_orig$a2c),k=3,endrule = "median")

result_orig=read.csv('C:/jiaocai/Other/FinModelOutput/OFO_param.csv')

result_orig$date=as.Date(result_orig$date)

OFE_OFO_merged=merge(x=result_equity_no_apc,y=result_orig,by='date')

#OFE_OFO_merged=OFE_OFO_merged[,-c(2)]

OFE_OFO_merged=OFE_OFO_merged[,c(1,3,4,5,6,21,22,23,24,26,27,36,37)]

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