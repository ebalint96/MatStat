data = read.csv("~/Dokumentumok/stat/weather_data.csv",sep=",",header=T)
data <- data[,-c(6,8,10,12,14,16,18,20,21,22,23,24)]
for (column in 6:12) {
  for (i in (1+1):(nrow(data)-1)) {
    if (is.na(data[i,column])) {
      data[i,6] = mean(data[(i-1):(i+1),column], na.rm=TRUE)
    }
    if (is.na(data[i,column])) {
      data[i,6] = mean(data[(i-2):(i+2),column], na.rm=TRUE)
    }
    if (is.na(data[i,column])) {
      data[i,6] = mean(data[(i-3):(i+3),column], na.rm=TRUE)
    }
  }
}

data_daily = aggregate(data[, 6:12], list(data$Day,data$Month,data$Year), mean, na.rm=TRUE)

plot(c(1:nrow(data_daily)),data_daily$Wind.Dir..10s.deg.,type = 'l')

