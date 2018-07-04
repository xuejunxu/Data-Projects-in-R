library("readstata13")
library("roll")
#library('zoo')
library('sqldf')
#Rewrite calculation of stat1 and stat2 in R

#read from local drive
#his_beta_all_stat=read.dta13("C:/jiaocai/Other/Econometric Models/Ronn_ForwardBetas/Ronn_ForwardBetas/Stata/Equity Beta/Data/hist_beta_err_all.dta")

hist_beta_err_all=read.dta13("C:/jiaocai/Other/Econometric Models/Jun_26th_Meeting/stat1 stata file/stata_code/Equity Beta/Data/hist_beta_err_all.dta")
write.csv(x=hist_beta_err_all,file='hist_beta_err_all.csv')

#do some minor modification on cop data
cop_data=read.dta13("C:/jiaocai/Other/Econometric Models/Jun_26th_Meeting/stat1 stata file/stata_code/Equity Beta/Data/cop.dta")

cop_data$L=NULL

save.dta13(cop_data,'C:/jiaocai/Other/Econometric Models/stata_code_xiang/Equity Beta/Data/cop.dta')

#read data of eight market using a for loop
namelist=c('apa','apc','cop', 'cvx', 'hes', 'mro', 'oxy', 'xom')

market_df <- data.frame(Doubles=double(),
                 Ints=integer(),
                 Factors=factor(),
                 Logicals=logical(),
                 Characters=character(),
                 stringsAsFactors=FALSE)

market_df_all<-data.frame(Doubles=double(),
                 Ints=integer(),
                 Factors=factor(),
                 Logicals=logical(),
                 Characters=character(),
                 stringsAsFactors=FALSE)

for (v in namelist){
	readfile=paste("C:/jiaocai/Other/Econometric Models/stata_code_xiang/Equity Beta/Data/",v,'.dta',sep='')
	#print(readfile)
	temp_df=read.dta13(readfile)
	temp_df=temp_df[,c(1,10,12)]
	spx_df=read.dta13("C:/jiaocai/Other/Econometric Models/stata_code_xiang/Equity Beta/Data/spx.dta")
	spx_df=spx_df[,c(1,12)]
	names(temp_df)=c('date','name','ret')
	names(spx_df)=c('date','ret_spx')
	temp_df_1=merge(x=temp_df,y=spx_df,by='date')
	market_df=rbind(market_df,temp_df_1)
}

# write a loop to enclose all data
for (v in namelist){
	readfile=paste("C:/jiaocai/Other/Econometric Models/stata_code_xiang/Equity Beta/Data/",v,'.dta',sep='')
	#print(readfile)
	temp_df=read.dta13(readfile)
	spx_df=read.dta13("C:/jiaocai/Other/Econometric Models/stata_code_xiang/Equity Beta/Data/spx.dta")
	spx_df=spx_df[,c(1,12)]
	names(spx_df)=c('date','ret_spx')
	temp_df_1=merge(x=temp_df,y=spx_df,by='date')
	market_df_all=rbind(market_df_all,temp_df_1)
}

write.csv(x=market_df_all,file='market_df_all.csv')

#read spx data 
#spx_data=read.dta13("C:/jiaocai/Other/Econometric Models/Jun_26th_Meeting/stat1 stata file/stata_code/Equity Beta/Data/spx.dta")

#sort dataframe by date and name
market_df=market_df[with(market_df, order(date,name)),]

market_df=market_df[-c(1:8),]

#rolling window regression: regress ret on ret_spx
#rolling_model=roll_lm(x=as.matrix(market_df$ret_spx),y=as.matrix(market_df$ret),width=60)
rr <- rollapply(market_df, width = 60,FUN = function(z) coef(lm(ret ~ ret_spx, data = as.data.frame(z))),by.column = FALSE, align = "right")

########################################################
# try it on STATA
########################################################

hist_data_all<-read.dta13('C:/jiaocai/Other/Econometric Models/stata_code_xiang/Equity Beta/Data/hist_beta_err_all.dta')

cal_stat1_new= function(sig,his_vol){
	t1=(sig/his_vol)^2
	return (mean(t1))
}

stat1_OFE_test = hist_beta_err_all %>% group_by(date) %>% summarise(cal_stat1_new(sig,var_ret))
names(stat1_OFE_test)=c('date','stat1')
stat1_OFE_test$stat1=stat1_OFE_test$stat1/8

stat1_OFE_test=merge(x=stat1_OFE_test,y=all_stat_OFE,by='date')
names(stat1_OFE_test)=c('date','stat1_calculated','stat2','stat1_file')
stat1_OFE_test$diff=stat1_OFE_test$stat1_calculated-stat1_OFE_test$stat1_file


##################### June 29th #################################################################
###  Calculate stat1 of One Factor Oil model

var_ret<-read.dta13('C:/jiaocai/Other/Econometric Models/Jun_26th_Meeting/hist_beta_err_all.dta')

sig_OFO<-subset(merged_crude, maturity==2)

save.dta13(sig_OFO,"sig_OFO.dta")

spx_ret_data=spx_data[,c(1,12)]

names(spx_ret_data)=c('date','ret_spx')

sig_OFO_data=merge(x=spx_ret_data,y=sig_OFO,by='date')

sig_OFO_data=sig_OFO_data[,c(1,2,9,11)]

save.dta13(sig_OFO_data,"sig_OFO_data.dta")

hist_var_OFO=read.dta13('hist_var_OFO.dta')

hist_var_OFO=merge(x=hist_var_OFO,y=sig_OFO_data,by='date')

hist_var_OFO$sig2=(hist_var_OFO$sig)*(hist_var_OFO$sig)

hist_var_OFO$stat1=hist_var_OFO$sig2/hist_var_OFO$var_ret

OFO_stat1=hist_var_OFO[,c(1,7)]

write.csv(x=OFO_stat1,file='OFO_stat1.csv')

### Calculate stat1 of Two Factor model

sig_two_factor=subset(nls_data,maturity==2)

sig_two_factor=sig_two_factor[,c(1,3,5,14,15)]

colnames(sig_two_factor)[4]='sig' #the sigma have been annualized

save.dta13(sig_two_factor,"sig_two_factor.dta")

# split the above sig_two_factor file to 8 files by market names
apc_two=subset(sig_two_factor, name=='apc')
save.dta13(apc_two,"C:/jiaocai/Other/Econometric Models/stata_code_xiang/Two Factor/apc.dta")

cop_two=subset(sig_two_factor, name=='cop')
save.dta13(cop_two,"C:/jiaocai/Other/Econometric Models/stata_code_xiang/Two Factor/cop.dta")

xom_two=subset(sig_two_factor, name=='xom')
save.dta13(xom_two,"C:/jiaocai/Other/Econometric Models/stata_code_xiang/Two Factor/xom.dta")

apa_two=subset(sig_two_factor, name=='apa')
save.dta13(apa_two,"C:/jiaocai/Other/Econometric Models/stata_code_xiang/Two Factor/apa.dta")

mro_two=subset(sig_two_factor, name=='mro')
save.dta13(mro_two,"C:/jiaocai/Other/Econometric Models/stata_code_xiang/Two Factor/mro.dta")

oxy_two=subset(sig_two_factor, name=='oxy')
save.dta13(oxy_two,"C:/jiaocai/Other/Econometric Models/stata_code_xiang/Two Factor/oxy.dta")

hes_two=subset(sig_two_factor, name=='hes')
save.dta13(hes_two,"C:/jiaocai/Other/Econometric Models/stata_code_xiang/Two Factor/hes.dta")

cvx_two=subset(sig_two_factor, name=='cvx')
save.dta13(cvx_two,"C:/jiaocai/Other/Econometric Models/stata_code_xiang/Two Factor/cvx.dta")

# generate sta files using STATA
# check the files using R

hist_err_apa=read.dta13("C:/jiaocai/Other/Econometric Models/stata_code_xiang/Two Factor/hist_beta_err_apa.dta")

# join stat1 data with existing data frame
stat1_twofac_df=read.dta13("C:/jiaocai/Other/Econometric Models/stata_code_xiang/Two Factor/two_factor_stat1.dta")

stat1_twofac_df=merge(x=crit_day_OFO_two,y=stat1_twofac_df,by='date',all.x=TRUE)

stat1_twofac_df=stat1_twofac_df[,c(1,13)]

names(stat1_twofac_df)=c('date','stat1_two_factor')

write.csv(x=stat1_twofac_df,file='stat1_two_factor_v1.csv')

# join two factor stat1 with existing data frame
stat1_twofac_df1=read.dta13("C:/jiaocai/Other/Econometric Models/stata_code_xiang/Two Factor/two_factor_stat1.dta")

stat1_twofac_df1=merge(x=crit_day_OFE_two_factor,y=stat1_twofac_df1,by='date',all.x=TRUE)

stat1_twofac_df=stat1_twofac_df[,c(1,13)]

names(stat1_twofac_df)=c('date','stat1_two_factor')

write.csv(x=stat1_twofac_df,file='stat1_two_factor_v2.csv')