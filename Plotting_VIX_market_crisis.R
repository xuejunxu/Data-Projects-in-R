
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
