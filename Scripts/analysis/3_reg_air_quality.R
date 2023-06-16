library(tidyverse)   
library(fixest)
library(splines2)

data_path <- "~/Data"

#----------------------------------------------------------------------------------
#  to produce the regression results of air quality impacts (used to plot figure 3)
#----------------------------------------------------------------------------------
monitor_emis <- readRDS(paste0(data_path, "/input/monitor_drought_emis.rds")) 

nox_so2_f <- paste0("nonsmoke_pm25~", 
                    paste(paste(c("within50km","dist_50_100km","dist_100_200km","dist_200_500km","above_500km"),"so2nox",sep="_"), collapse = "+"))

met_all <- "bSpline(temp_2m, df=4) + bSpline(precep_max, df=4) + bSpline(dewpoint_2m, df=4) + bSpline(PBLH, df=4)  + bSpline(sea_level_pressure, df=4) + bSpline(surface_pressure, df=4) + bSpline(u10m, df=4)  + bSpline(v10m, df=4) + bSpline(wind_speed, df=4) "

nox_so2_bin0 <- feols(fml = as.formula(paste(nox_so2_f,"+",met_all,"| state^year + month +monitor_id")), data=monitor_emis, cluster="state^year^month", weights=monitor_emis$var_so2nox)
nox_so2_bin1 <- feols(fml = as.formula(paste(nox_so2_f,"| state^year + month +monitor_id")), data=monitor_emis, cluster="state^year^month", weights=monitor_emis$var_so2nox)
nox_so2_bin2 <- feols(fml = as.formula(paste(nox_so2_f,"+",met_all,"| state[year] + month +monitor_id")), data=monitor_emis, cluster="state^year^month", weights=monitor_emis$var_so2nox)
nox_so2_bin3 <- feols(fml = as.formula(paste(nox_so2_f,"+",met_all,"| state^year + month +monitor_id")), data=monitor_emis, cluster="state^year^month")
nox_so2_bin4 <- feols(fml = as.formula(paste(nox_so2_f,"+",met_all,"| monitor_id[year] + month +monitor_id")), data=monitor_emis, cluster="state^year^month", weights=monitor_emis$var_so2nox)

dist_bins <- c("<50km","50-100","100-200","200-500",">500km")
coef_bins <- rbind.data.frame(data.frame(coef=coef(nox_so2_bin0)[1:length(dist_bins)], se=summary(nox_so2_bin0)$se[1:length(dist_bins)], model="State-year FE", distance=dist_bins),
                              data.frame(coef=coef(nox_so2_bin1)[1:length(dist_bins)], se=summary(nox_so2_bin1)$se[1:length(dist_bins)], model="No met", distance=dist_bins),
                              data.frame(coef=coef(nox_so2_bin2)[1:length(dist_bins)], se=summary(nox_so2_bin2)$se[1:length(dist_bins)], model="State trend",distance=dist_bins),
                              data.frame(coef=coef(nox_so2_bin3)[1:length(dist_bins)], se=summary(nox_so2_bin3)$se[1:length(dist_bins)], model="Unweighted",distance=dist_bins),
                              data.frame(coef=coef(nox_so2_bin4)[1:length(dist_bins)], se=summary(nox_so2_bin4)$se[1:length(dist_bins)], model="Monitor trend",distance=dist_bins))

coef_bins$distance <- factor(coef_bins$distance, levels=dist_bins)
write.csv(coef_bins, paste0(data_path, "/intermediate/pm25_reg_results.csv"))


