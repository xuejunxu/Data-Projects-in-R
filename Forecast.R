############################ July 6th
### CPFC data from Bloomberg
cpfc_data<-read.csv('C:/jiaocai/Other/Econometric Models/xuejun_CPFC_data.csv')

#try to modify the format of dates
cpfc_data$med_date=strptime(cpfc_data$Date_med,format="%m/%d/%Y")
cpfc_data$curr_date=strptime(cpfc_data$Date_Curr,format="%m/%d/%Y")

median_4q<-cpfc_data[,c(6,3)]

curr_4q<-cpfc_data[,c(7,5)]

names(median_4q)=c('date','median_price')

names(curr_4q)=c('date','curr_price')

#merge the two data frames by Date, outer join
curr_4q_merge=merge(x=median_4q,y=curr_4q,by='date',all.x=TRUE,all.y=TRUE)

curr_4q_merge=curr_4q_merge[1:767,]

#calculate the difference of median - current
curr_4q_merge$diff=curr_4q_merge$median_price-curr_4q_merge$curr_price

write.csv(x=curr_4q_merge,file='four_quarter_forecast.csv')


##### read CL13 data
cl13<-read.csv("C:/jiaocai/Other/Econometric Models/July_3rd Meeting/CL13_160321.csv",header=FALSE)
enrfcl<-read.csv("C:/jiaocai/Other/Econometric Models/July_3rd Meeting/ENRFCL_160321.csv",header=FALSE)

names(cl13)=c('date','cl13')
names(enrfcl)=c('date','enrfcl')

cl13$date=as.Date(cl13$date,format="%m/%d/%Y")
enrfcl$date=as.Date(enrfcl$date,format="%m/%d/%Y")

merge_cl13_enrfcl=merge(x=cl13,y=enrfcl,by='date',all.y=TRUE,all.x=TRUE)

write.csv(x=merge_cl13_enrfcl,file='merge_cl13_enrfcl.csv')


#### read CL1 data
cl1<-read.csv("C:/jiaocai/Other/Econometric Models/July_3rd Meeting/CL1 comd.csv",header=FALSE)
names(cl1)=c('date','cl1')

spx_ind<-read.csv("C:/jiaocai/Other/Econometric Models/July_3rd Meeting/SPX_idx.csv",header=FALSE)
names(spx_ind)=c('date','spx')

cl1$date=as.Date(cl1$date,format="%m/%d/%Y")
spx_ind$date=as.Date(spx_ind$date,format="%m/%d/%Y")
nrow(cl1)
nrow(spx_ind)

merge_cl1_spx=merge(x=cl1,y=spx_ind,by='date',all.x=TRUE,all.y=TRUE)

write.csv(x=merge_cl1_spx,file='merge_cl1_spx.csv')

#### use R to let two excel files lie in the same date range
ovx_from_14<-read.csv("C:/jiaocai/Other/Econometric Models/July_3rd Meeting/OVX_from_140801.csv",header=FALSE)
names(ovx_from_14)=c('date','ovx')
ovx_from_14$date=as.Date(ovx_from_14$date,format="%m/%d/%Y")

enrfcl_from_14<-read.csv("C:/jiaocai/Other/Econometric Models/July_3rd Meeting/enrfcl_from_140801.csv",header=FALSE)
names(enrfcl_from_14)=c('date','enrfcl')
enrfcl_from_14$date=as.Date(enrfcl_from_14$date,format="%m/%d/%Y")

nrow(ovx_from_14)
nrow(enrfcl_from_14)
merge_ovx_enr=merge(x=ovx_from_14,y=enrfcl_from_14,by='date',all.y=TRUE)
write.csv(x=merge_ovx_enr,file='merge_check_ovx_enr.csv')