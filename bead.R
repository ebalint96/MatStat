data = read.csv("https://raw.githubusercontent.com/ebalint96/MatStat/master/weather_data.csv",sep=",",header=T)
data <- data[,-c(6,8,10,12,14,16,18,20,21,22,23,24)]
for (column in 6:12) {
  for (i in (1+1):(nrow(data)-1)) {
    if (is.na(data[i,column])) {
      data[i,6] = mean(data[(i-5):(i+5),column], na.rm=TRUE)
    }
    if (is.na(data[i,column])) {
      data[i,6] = mean(data[,column], na.rm=TRUE)
    }
  }
}

data_daily = aggregate(data[, 6:12], list(data$Day,data$Month,data$Year), mean, na.rm=TRUE)


plot(c(1:nrow(data_daily)),data_daily$Temp...C.,type = 'l')

summary(data)

generate_statistics <- function(input_data_frame,columns,group_by){
  total <- unique(data[group_by])
  group_by_list = list()
  group_by_names = c();
  i = 1
  for (group in group_by) {
    group_by_list[[i]] = input_data_frame[,group]
    append(group_by_names, colnames(input_data_frame)[group])
    i = i + 1
  }
  print(group_by_names)
  for (column in columns) {
    Minimum = aggregate(input_data_frame[, column], group_by_list, min, na.rm=TRUE)
    colnames(Minimum)[ncol(Minimum)] <- paste("Minimum", colnames(input_data_frame)[column], sep=" ")
    print(Minimum)
    total <- cbind(total,rev(Minimum)[1])

    Maximum = aggregate(input_data_frame[, column], group_by_list, max, na.rm=TRUE)
    colnames(Maximum)[ncol(Maximum)] <- paste("Maximum", colnames(input_data_frame)[column], sep=" ")
    total <- cbind(total,rev(Maximum)[1])

    Mean = aggregate(input_data_frame[, column], group_by_list, mean, na.rm=TRUE)
    colnames(Mean)[ncol(Mean)] <- paste("Mean", colnames(input_data_frame)[column], sep=" ")
    total <- cbind(total,rev(Mean)[1])
    
    Median = aggregate(input_data_frame[, column], group_by_list, median, na.rm=TRUE)
    colnames(Median)[ncol(Median)] <- paste("Median", colnames(input_data_frame)[column], sep=" ")
    total <- cbind(total,rev(Median)[1])
  }
  return(total)
}

stat = generate_statistics(data,c(6:12),c(3,2))

