XU_XUEJUN_Rfunction<-function(){
	
path<-getwd()

#read file from current directory
eia<-read.csv("RHODES_JOSHUA_EIA_2016_data.csv")

#aggregate nameplate capacity by average operating year(rounded)
e4 <- aggregate(eia$Nameplate.Capacity, by = list(round(eia$Operating.Year.avg, -1), eia$Technology), FUN = sum)
names(e4) <- c('year', 'tech', 'MW_cap')

e5 <- reshape(data = e4, idvar = "tech", timevar = "year", direction = "wide")

e6 <- e5
e6[is.na(e6)]<-0
e6$techSum <- rowSums(e6[,-1], na.rm = T)
e6 <- e6[e6$techSum > 2000,]
e6 <- e6[,-ncol(e6)]

e7 <- e6[,-1]
e7 <- e7[ , order(names(e7))]

dat <- cbind(e6[,1], e7)
names(dat)[1] <- 'tech'

pdf(file = 'XU_XUEJUN_US_CAPACITY_STACKEDBAR.pdf', height = 6, width = 9)
#simple stacked barplot
set.seed(2)
#cols <- distinctColorPalette(nrow(e6))
cols <- colors()[round(runif(nrow(e6), min = 0, max = length(colors())))]

par(mar = c(5.1, 6.1, 4.1, 2.1))
mids <- barplot(as.matrix(dat[2:14]/1000), las = 1, col = cols, xaxt = 'n', yaxt = 'n', xlab = '')

axis(side = 1, cex.axis = 1.25, at = mids, labels = seq(from = 1900, to = 2020, by = 10)) 
axis(side = 2, cex.axis = 1.5, at = seq(from = 0, to = 250, by = 50), labels = seq(from = 0, to = 250, by = 50), las = 1) 

mtext(side = 1, cex = 2, line = 3, text = 'Decade ending')
mtext(side = 2, line = 4, cex = 2, text = 'Capacity (GW)')
mtext(side = 3, line = 0, cex = 2, text = 'Capacity (GW) added per decade to the\n US grid by type')

legend('topleft', legend = e6$tech, fill = cols, cex = 1, bty = 'n') ## add a legend

dev.off()
}