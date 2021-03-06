
XU_XUEJUN_OPT_LP<-function(){
path<-getwd()

#part 1
eiadata<-read.csv("RHODES_JOSHUA_EIA_2016_data.csv",header=TRUE)
fuel<-read.csv("fuel_properties.csv",header=TRUE)

#set demand and emission_cap
Demand<-550000
Emission<-350000

#select the subsets
#have a capacity greater than 50
eiadata<-subset(eiadata,eiadata$Nameplate.Capacity>50)
#have a heat rate greater than 0
eiadata<-subset(eiadata,eiadata$Heat.Rate>0)
#have a heat rate less than 20,000
eiadata<-subset(eiadata,eiadata$Heat.Rate<20000)
#use the following fuel types only:
#BIT, BLQ, DFO, LIG, NG, NUC, RC, RFO, SUB, WC, WDS
eiadata<-subset(eiadata,eiadata$Energy.Source.1 %in% c('BIT', 'BLQ', 'DFO', 'LIG', 'NG', 'NUC', 'RC', 'RFO', 'SUB', 'WC', 'WDS'))
fuel<-subset(fuel,fuel$energy_name %in% c('BIT', 'BLQ', 'DFO', 'LIG', 'NG', 'NUC', 'RC', 'RFO', 'SUB', 'WC', 'WDS'))
origin_col_names<-names(eiadata)

#calculate operating cost Cg to each generator
merged_eia<-merge(x=eiadata,y=fuel,by.x='Energy.Source.1',by.y='energy_name',all.x=TRUE)
merged_eia$Elec.Fuel.Consumption.MMBtu<-as.numeric(merged_eia$Elec.Fuel.Consumption.MMBtu)
merged_eia$Cg<-(merged_eia$Elec.Fuel.Consumption.MMBtu)*(merged_eia$dol_per_mmBtu)/(merged_eia$Net.Generation.MWh)

#calculate emission rate of Rg
merged_eia$Rg<-(merged_eia$Elec.Fuel.Consumption.MMBtu)*(merged_eia$kg_CO2_per_mmBtu)/(merged_eia$Net.Generation.MWh)/1000
m1<-merged_eia

library(lpSolveAPI)
#start building the LP model
lp_model<-make.lp(2,nrow(m1))
write.lp(lp_model,'xu_xuejun_opt.lp',type='lp')

#fill in the columns of the LP
first_row<-as.vector(rep(1,nrow(m1)))
second_row<-as.vector(m1$Rg)
set.row(lp_model,1,first_row)
set.row(lp_model,2,second_row)

#set the objective function of Lp model
obj_func<-as.vector(m1$Cg)
set.objfn(lp_model,obj_func)

#set the rhs constraints of LP model
set.rhs(lp_model, c(Demand,Emission))
set.constr.type(lp_model, c(">=", "<="))

#set the upper bounds and lower bounds of decision variables
set.bounds(lp_model,lower=as.vector(rep(0,nrow(m1))))
set.bounds(lp_model,upper=as.vector(m1$Nameplate.Capacity))

#set the objective function to be minimize
lp.control(lp_model,sense='min')

write.lp

#solve the linear programming model
solve(lp_model)

#print out the objective function
obj<-get.objective(lp_model)
print(paste("The objective of the problem is ",obj,"$",sep=""))

#print out the average cost of satisfying demand
print(paste("The average cost of generation is ",obj/Demand,"$/MWh",sep=""))

#print out the constraints
cons<-get.constraints(lp_model)
print(paste("The total amount of power generated is ",cons[1],"MWh",sep=""))
print(paste("The total amount of co2 emitted is ",cons[2],"tons",sep=""))
 
#print out the amount of energy generated by fuel type
lpvar<-get.variables(lp_model)
m1$ene_gen<-lpvar
agg<-aggregate(m1$ene_gen,by=list(m1$Energy.Source.1),FUN=sum)
names(agg)<-c('Energy.Source.1','Total.Energy.Generated')
print("Table of Generation per Fuel Type")
print(agg)

#part2
#write a loop to solve for different values of Eg
Eg.Seq<-seq(350000,230000,by=-10000)

#Create an empty data frame

result.frame <- data.frame(Doubles=double(),
                           Ints=integer(),
                           Factors=factor(),
                           Logicals=logical(),
                           Characters=character(),
                           stringsAsFactors=FALSE)

#use result.frame to store the results of the lp model
for (Eg.i in Eg.Seq){
	set.rhs(lp_model,c(Demand,Eg.i))
	solve(lp_model)
	obj.i<-t(as.vector(get.objective(lp_model)))
	con.i<-t(as.vector(get.constraints(lp_model)))
	var.i<-t(as.vector(get.variables(lp_model)))
    result<-cbind(obj.i,con.i,var.i)
    result.frame<-rbind(result.frame,result)
}

#print out the amount of energy generated by fuel type for new constraints
row_num<-1
for (Eg.i in Eg.Seq){
	get.result<-result.frame[row_num,]
	row_num<-row_num+1
	var<-as.vector(get.result)
	var<-var[4:length(var)]
	var<-as.numeric(var)
	m1$Updated.eng<-var
	upd_agg<-aggregate(m1$Updated.eng,by=list(m1$Energy.Source.1),FUN=sum)
    names(upd_agg)<-c('Energy.Source.1','Total.Energy.Generated')
    print(paste("When the Emission is ",Eg.i,"tons, Table of Generation per Fuel Type:", sep=""))
    print(upd_agg)
}

#print the bar chart showing the amount of energy generated by fuel type for new constraints
row_num<-1
for (Eg.i in Eg.Seq){
	get.result<-result.frame[row_num,]
	row_num<-row_num+1
	var<-as.vector(get.result)
	var<-var[4:length(var)]
	var<-as.numeric(var)
	m1$Updated.eng<-var
	upd_agg<-aggregate(m1$Updated.eng,by=list(m1$Energy.Source.1),FUN=sum)
    names(upd_agg)<-c('Energy.Source.1','Total.Energy.Generated')

}

#Bar Plot of changes between high emissions and low emissions
high.result<-as.vector(result.frame[1,])
high.result<-as.numeric(high.result[4:length(high.result)])
m1$high.eng<-high.result
high_agg<-aggregate(m1$high.eng,by=list(m1$Energy.Source.1),FUN=sum)
names(high_agg)<-c('Energy.Source.1','Total.Energy.Generated')

low.result<-as.vector(result.frame[dim(result.frame)[1],])
low.result<-as.numeric(low.result[4:length(low.result)])
m1$low.eng<-low.result
low_agg<-aggregate(m1$low.eng,by=list(m1$Energy.Source.1),FUN=sum)
names(low_agg)<-c('Energy.Source.1','Total.Energy.Generated')

high.low<-cbind(high_agg,low_agg)
high.low$diff<-high.low[,4]-high.low[,2]

#output the barplot as a pdf file
pdf(file = 'XU_XUEJUN_FUEL_CHANGE.pdf', height = 6, width = 9)
#barplot(high.low$diff, main="Bar Chart of Energy Generated By Fuel", xlab="Fuel Type",col='violet')
diffplot<-barplot(high.low$diff, main="Bar Chart of Energy Generated By Fuel", xlab="Fuel Type",col='violet')

mtext(text = 'Change in Dispatch from High to Low Emission Cap', side = 2, line = 3)
axis(side = 1, at=diffplot[c(1:11)], labels =c('BIT', 'BLQ', 'DFO', 'LIG', 'NG', 'NUC', 'RC', 'RFO', 'SUB', 'WC', 'WDS'),las = 1) 

dev.off()

#plot the dispatch cost for different emission constraints
cost.emi<-result.frame[,1]
costdata<-as.data.frame(cbind(Eg.Seq,cost.emi))
costdata$avgcost<-costdata$cost.emi/Demand
costdata$cost.emi<-NULL 

#output the plot as png file 
png(file = 'XU_XUEJUN_COST_CHANGE.png')
plot(costdata,avgcost~Eg.Seq,type='l',col='violet',main='Average Cost of Meeting Demand with Variant Emission Capacity')
mtext(text = 'Emission Cap(tons)', side = 1, line = 2)
mtext(text = 'Dispatch Cost($/MWh)',side =2, line = 2)

dev.off()

}