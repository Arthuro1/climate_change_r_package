rm(list = ls())

heatData_us=read.csv("./data_old/heat-wave-index-usa.csv", sep=",")
heatData_us = heatData_us[heatData_us[,"Year"] >= 1901,c(4)]

droughtData=read.csv("data_old/drought-severity-index-us.csv", sep=",")
droughtData_us = droughtData[droughtData[,1] == "United States",]
droughtData_us = droughtData_us[droughtData_us[,"Year"] >= 1901,c(3,5)]

#temperatureData_us=read.csv("data_old/temperature-us.csv", sep=",")
temperatureData_us = read.csv("data_old/temperature-us-new.csv", sep=",")
temperatureData_us = temperatureData_us[temperatureData_us[,"Date"] >= 190112,c(2)]

#co2Data=read.csv("data_old/co-emissions-per-capita.csv", sep=",")
co2Data = read.csv("data_old/annual-co2-emissions.csv", sep=",")
co2Data_us = co2Data[co2Data[,1] == "United States",]
co2Data_us = co2Data_us[co2Data_us[,"Year"] >= 1901,c(4)]
co2Data_us = co2Data_us/max(co2Data_us)

#co2Data_de_new = co2Data_new[co2Data_new[,1] == "Germany",]
#co2Data_de_new = co2Data_de_new[co2Data_de_new[,"Year"] >= 1901,c(4)]
#co2Data_de_new = co2Data_de_new/max(co2Data_de_new)

climateset = cbind(droughtData_us, co2Data_us, temperatureData_us, heatData_us)

#sorted_co2_de_data <- sort(co2Data_de_new, index.return = TRUE)
#plot(sorted_co2_de_data$x, co2Data_us_new[sorted_co2_de_data$ix], type = "l")
