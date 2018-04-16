Xuejun_Xu_Hw1<-function(month,day,year,station_id){
  library(lubridate)
  month<-as.character(month)
  day<-as.character(day)
  year<-as.character(year)
  xxurl1<-paste('https://www.wunderground.com/weatherstation/WXDailyHistory.asp?ID=',station_id,'&month=',month,'&day=',day,'&year=',year,'&format=1',sep='')
  #xu_url <- 'https://www.wunderground.com/weatherstation/WXDailyHistory.asp?ID=KTXAUSTI942&month=1&day=16&year=2018&format=1'
  data<-read.csv(xxurl1, row.names = NULL)
  data <- data[!duplicated(data$row.names),]
  data <- data[data$row.names != '<br>',]
  col_names <- names(data)[2:length(names(data))]
  names(data) <- col_names
  num_real_col <- dim(data)[2] - 1
  data <- data[c(1:num_real_col)]
  data <- as.data.frame(cbind(as.character(data$Time), data$TemperatureF, data$DewpointF, data$WindSpeedMPH, data$Humidity))
  names(data) <- c('time', 'tempF', 'DewpointF', 'wnd.spd.mph', 'RH')
  data$tempC <- (as.numeric(as.character(data$tempF)) - 32)*(5/9)
  data$DP_tempC <- (as.numeric(as.character(data$DewpointF)) - 32)*(5/9)
  data$wnd.spd.ms <- as.integer(as.character(data$wnd.spd.mph))*0.44704
  data$RH <- as.numeric(as.character(data$RH))*1
  data <- data[c(1, 5, 6, 7, 8)]
  data$time <- as.POSIXct(data$time)
  data$time_ceiling <- ceiling_date(data$time, "hour")
  data <- aggregate(data[c(2:5)], by = list(data$time_ceiling), FUN = mean)
  names(data) <- c('time', 'RH', 'tempC', 'DP_tempC', 'wnd.spd.ms')
  data$wetbulb_tempC <- (-5.806+0.672*data$tempC-0.006*data$tempC*data$tempC+(0.061+0.004*data$tempC+0.000099*data$tempC*data$tempC)*data$RH+(-0.000033-0.000005*data$tempC-0.0000001*data$tempC*data$tempC)*data$RH*data$RH)	
  return(data)
}
