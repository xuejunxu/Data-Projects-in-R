XU_XUEJUN_HWK2_EIA<-function(year){

#set working directory
yearnum<-as.numeric(year)
path<-getwd()
setwd(paste(path,'/ME397_A2_data_csv',sep = ''))

#create index for reading csv files
f860index<-paste('F860_',yearnum,'.csv',sep='')
f923index<-paste('F923_',yearnum,'.csv',sep='')

#read csv file f860,f923
f860<-read.csv(f860index,encoding='UTF-8',stringsAsFactors=FALSE)
f923<-read.csv(f923index,encoding='UTF-8',stringsAsFactors=FALSE)

#For f860 files, adjust the column names
if(yearnum==2007|yearnum==2008){
	colnames(f860)[1]<-'Utility.Name'
	colnames(f860)[colnames(f860)=='UTILCODE']<-'Utility.ID'
	colnames(f860)[colnames(f860)=='PLNTCODE']<-'Plant.Code'
	colnames(f860)[colnames(f860)=='PLNTNAME']<-'Plant.Name'
	colnames(f860)[colnames(f860)=='PRIMEMOVER']<-'Prime.Mover'
	colnames(f860)[colnames(f860)=='ENERGY_SOURCE_1']<-'Energy.Source.1'
	colnames(f860)[colnames(f860)=='NAMEPLATE']<-'Nameplate.Capacity'
	colnames(f860)[colnames(f860)=='OPERATING_YEAR']<-'Operating.Year'
}

if(yearnum==2009|yearnum==2010|yearnum==2011){
	colnames(f860)[1]<-'Utility.ID'
	colnames(f860)[colnames(f860)=='UTILITY_NAME']<-'Utility.Name'
	colnames(f860)[colnames(f860)=='PLANT_CODE']<-'Plant.Code'
	colnames(f860)[colnames(f860)=='PLANT_NAME']<-'Plant.Name'
	colnames(f860)[colnames(f860)=='PRIME_MOVER']<-'Prime.Mover'
	colnames(f860)[colnames(f860)=='ENERGY_SOURCE_1']<-'Energy.Source.1'
	colnames(f860)[colnames(f860)=='NAMEPLATE']<-'Nameplate.Capacity'
	colnames(f860)[colnames(f860)=='OPERATING_YEAR']<-'Operating.Year'
}

if(yearnum>=2012){
	colnames(f860)[1]<-'Utility.ID'
	colnames(f860)[colnames(f860)=='Nameplate.Capacity..MW.']<-'Nameplate.Capacity'
}

#For f923 files, adjust the column names
colnames(f923)[1]<-'Plant.Code'
colnames(f923)[4]<-'Plant.Name'
colnames(f923)[5]<-'Utility.Name'
colnames(f923)[6]<-'Utility.ID'
colnames(f923)[14]<-'Prime.Mover'
colnames(f923)[15]<-'Energy.Source.1'
colnames(f923)[95]<-'Elec.Fuel.Consumption.MMBtu'
colnames(f923)[96]<-'Net.Generation.MWh'
colnames(f923)[97]<-'Year'

#Create Technology column for data from 2007-2013
if(yearnum>=2007&yearnum<=2013){
	f860.16<-read.csv('F860_2016.csv',encoding='UTF-8',stringsAsFactors=FALSE)
	colnames(f860.16)[1]<-'Utility.ID'
	colnames(f860)[colnames(f860)=='Nameplate.Capacity..MW.']<-'Nameplate.Capacity'
	f860.160<-f860.16[c('Prime.Mover','Energy.Source.1','Technology')]
	unicom<-unique(f860.160[c('Prime.Mover','Energy.Source.1','Technology')])
	f860m<-merge(x=f860,y=unicom,by=c('Prime.Mover','Energy.Source.1'),all.x=TRUE)
	f860<-f860m
}

# select the subsets of useful columns from 2016 data
f860c<-f860[c('Utility.Name','Plant.Name','Utility.ID','Plant.Code','Energy.Source.1','Prime.Mover','Technology','Nameplate.Capacity')]

#delete redundant rows 
f860c<-unique(f860c)
f860c<-subset(f860c,Nameplate.Capacity!='')
#change the data type into numeric, otherwise it cannot be aggregate by FUN=sum
#replace all the comma format in the data, and then transfer data int numeric
f860c$Nameplate.Capacity<-gsub(',','',f860c$Nameplate.Capacity)
f860c$Nameplate.Capacity<-as.numeric(as.character(f860c$Nameplate.Capacity))

#aggregate Nameplate.Capacity by FUN=sum
ag1<-as.data.frame(aggregate(x=f860c$Nameplate.Capacity,by=list(f860c$Utility.ID,f860c$Plant.Code,f860c$Prime.Mover,f860c$Energy.Source.1,f860c$Technology),FUN=sum))
names(ag1)<-c('Utility.ID','Plant.Code','Prime.Mover','Energy.Source.1','Technology','Nameplate.Capacity')

#Get the parameters of Operating.Year,avg,max,min
f860c2<-f860[c('Utility.Name','Plant.Name','Utility.ID','Plant.Code','Energy.Source.1','Prime.Mover','Technology','Operating.Year')]
f860c2<-unique(f860c2)
f860c2<-subset(f860c2,Operating.Year!='')
f860c2$Operating.Year<-as.numeric(as.character(f860c2$Operating.Year))
ag2<-as.data.frame(aggregate(x=f860c2$Operating.Year,by=list(f860c2$Utility.ID,f860c2$Plant.Code,f860c2$Prime.Mover,f860c2$Energy.Source.1,f860c2$Technology),FUN=function(x) c(avg=mean(x),max=max(x),min=min(x))))
f860c3<-as.data.frame(cbind(ag2[c('Group.1','Group.2','Group.3','Group.4','Group.5')],as.data.frame(as.matrix(ag2$x))))
names(f860c3)<-c('Utility.ID','Plant.Code','Prime.Mover','Energy.Source.1','Technology','Operating.Year.avg','Operating.Year.max','Operating.Year.min')

#merge the data of Operating.Year parameters and Nameplate.Capacity into one data.frame
m2<-merge(x=ag1,y=f860c3,by=c('Utility.ID','Plant.Code','Prime.Mover','Energy.Source.1','Technology'))

#get the useful columns from F923
f923c<-subset(f923, ,c(1,4:6,14,15,95,96,97))
f923c<-unique(f923c)
f923c<-subset(f923c,Net.Generation.MWh!='')
f923c<-subset(f923c,Elec.Fuel.Consumption.MMBtu!='')

#make sure all the variables in the data.frame are character variables, otherwise they cannot be merged
m2[,c(1,ncol(m2))] <- sapply(m2[,c(1,ncol(m2))],as.character)
f923c[,c(1,ncol(f923c))] <- sapply(f923c[,c(1,ncol(f923c))],as.character)

#aggregate Net.Generation and Elec.Fuel Consumption by sum
f923c$Net.Generation.MWh<-gsub(',','',f923c$Net.Generation.MWh)
f923c$Net.Generation.MWh<-as.numeric(as.character(f923c$Net.Generation.MWh))
ag3<-aggregate(x=f923c$Net.Generation.MWh,by=list(f923c$Plant.Code,f923c$Utility.ID,f923c$Prime.Mover,f923c$Energy.Source.1),FUN=sum)
names(ag3)<-c('Plant.Code','Utility.ID','Prime.Mover','Energy.Source.1','Net.Generation.MWh')

f923c$Elec.Fuel.Consumption.MMBtu<-gsub(',','',f923c$Elec.Fuel.Consumption.MMBtu)
f923c$Elec.Fuel.Consumption.MMBtu<-as.numeric(as.character(f923c$Elec.Fuel.Consumption.MMBtu))
ag4<-aggregate(x=f923c$Elec.Fuel.Consumption.MMBtu,by=list(f923c$Plant.Code,f923c$Utility.ID,f923c$Prime.Mover,f923c$Energy.Source.1),FUN=sum)
names(ag4)<-c('Plant.Code','Utility.ID','Prime.Mover','Energy.Source.1','Elec.Fuel.Consumption.MMBtu')

#merge them and get all we needed from F923
m3<-merge(x=ag3,y=ag4,by.x=c('Utility.ID','Plant.Code','Prime.Mover','Energy.Source.1'),by.y=c('Utility.ID','Plant.Code','Prime.Mover','Energy.Source.1'))

#merge F860 and F923 data 
m4<-merge(x=m2,y=m3,by.x=c('Utility.ID','Plant.Code','Prime.Mover','Energy.Source.1'),by.y=c('Utility.ID','Plant.Code','Prime.Mover','Energy.Source.1'))

#getting the unique combination of Utility ID, Utility Name and Plant Code and Plant Name.
uniutl<-unique(f860c[,c('Utility.ID','Utility.Name')])
uniplt<-unique(f860c[,c('Plant.Code','Plant.Name')])

#Create new columns
m4$Utility.Name<-NA
m4$Plant.Name<-NA
m4$Year<-NA

#for every i in m4, get the corresponding Utility ID and Utility Name
newdata<-data.frame()
for(i in 1:nrow(m4)){
	newdata[,i]<-subset(uniutl,Utility.ID==m4$Utility.ID[i],select=Utility.Name)   
}

#transpose the newdata
newdt<-t(newdata)

#for every i in m4, get the corresponding Plant Code and Plant Name
for(k in 1:nrow(m4)){
	newdata[,k]<-subset(uniplt,Plant.Code==m4$Plant.Code[i],select=Plant.Name)   
}
#also transpose
newdt2<-t(newdata)

#assign values to the new columns
for(j in 1:nrow(m4)){
	m4$Utility.Name[j]<-newdt[j]
	m4$Plant.Name[j]<-newdt2[j]
    m4$Year[j]<-yearnum
}

#Calculate new columns
m4$Capacity.Factor<-m4$Net.Generation.MWh/(m4$Nameplate.Capacity*8760)
m4$Heat.Rate<-(m4$Elec.Fuel.Consumption.MMBtu * 1000)/m4$Net.Generation.MWh

#change the sequence of the columns
m4<-m4[c(1,2,3,4,12,5,13,6,7,8,9,10,11,15,16,14)]

#Calculate percentage
sumnp<-sum(m4$Nameplate.Capacity)/sum(as.numeric(f860c$Nameplate.Capacity))
sumnp<-format(x=100*sumnp,digits=4,format='f')
sumgm<-sum(m4$Net.Generation.MWh)/sum(as.numeric(f923c$Net.Generation.MWh))
sumgm<-format(x=100*sumgm,digits=4,format='f')

#print the output
print(paste('Nameplate.Capacity in final dataset:',sumnp,'%',sep=' '))
print(paste('Net.Generation.MWh in final dataset',sumgm,'%',sep=' '))

filename<-paste('XU_XUEJUN_EIA_',yearnum,'_DATA.csv',sep='')
write.csv(x=m4,file=filename,row.names=FALSE)

}

