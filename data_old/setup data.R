rm(list = ls())

heat_waves_index_us=read.csv("./data_old/heat-wave-index-usa.csv", sep=",")
heat_waves_index_us = heat_waves_index_us[heat_waves_index_us[,"Year"] >= 1901,c(4)]

drougth_severity_index=read.csv("data_old/drought-severity-index-us.csv", sep=",")
drougth_severity_index_us = drougth_severity_index[drougth_severity_index[,1] == "United States",]
drougth_severity_index_us = drougth_severity_index_us[drougth_severity_index_us[,"Year"] >= 1901,c(3,5)]

temperature_us = read.csv("data_old/temperature-us-new.csv", sep=",")
temperature_us = temperature_us[temperature_us[,"Date"] >= 190112,c(2)]

co2_emission = read.csv("data_old/annual-co2-emissions.csv", sep=",")
co2_emission_us = co2_emission[co2_emission[,1] == "United States",]
co2_emission_us = co2_emission_us[co2_emission_us[,"Year"] >= 1901,c(4)]

heavy_precipitation_area_us = read.csv("data_old/heavy-precip_2.csv")
heavy_precipitation_area_us = heavy_precipitation_area_us[heavy_precipitation_area_us[, "Year"] >= 1901, c(2)]

climateset = as.data.frame(cbind(drougth_severity_index_us, co2_emission_us, temperature_us, heat_waves_index_us, heavy_precipitation_area_us))
save(climateset, file = "data/climateset.RData")
