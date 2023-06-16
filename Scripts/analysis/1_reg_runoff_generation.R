library(tidyverse)   
library(lfe)

data_path <- "~/Data"

set.seed(100)
#----------------------------------------------------------------------
#  to produce the pooled regression results that are plotted in figure 2 
#---------------------------------------------------------------------
raw_data <- readRDS(paste0(data_path,"/input/runoff_generation_month_final.rds"))
data <- filter(raw_data, n_gen_nonmissing >= 4, grossload_mw!=0) ## only keep obs if the unit has >=4 non-missing obs in a given month

region_list <- c("CA","SW","NW")

FE <- " + year | month + orispl_unit | 0 | orispl"
control_var <- " + log(sales_mwh_total1) + wind_monthly + solar_monthly + plant_temp "
linear_runoff <- "runoff_9_anomaly_CA +  runoff_9_anomaly_NW +  runoff_9_anomaly_SW "

coef_combined <- NULL
nboot <- 500
for (rr in region_list){
  print(rr)
  data_region <- filter(data, region==rr)
  
  gen_0 <- lfe::felm(formula=as.formula(paste("log(grossload_mw) ~ ", linear_runoff, FE)), data=data_region, Nboot=nboot, weights=data_region$monthly_gen)
  gen_1 <- lfe::felm(formula=as.formula(paste("log(grossload_mw) ~ ", linear_runoff, control_var, FE)), data=data_region, Nboot=nboot, weights=data_region$monthly_gen)

  coef_tmp <- bind_rows(cbind.data.frame(coef=gen_0$boot[1,], region_sample=rr, region_drought="CA", bootid=1:nboot, ctrl_type="no ctrl"),
                        cbind.data.frame(coef=gen_0$boot[2,], region_sample=rr, region_drought="NW", bootid=1:nboot, ctrl_type="no ctrl"),
                        cbind.data.frame(coef=gen_0$boot[3,], region_sample=rr, region_drought="SW", bootid=1:nboot, ctrl_type="no ctrl"),
                        cbind.data.frame(coef=gen_1$boot[1,], region_sample=rr, region_drought="CA", bootid=1:nboot, ctrl_type="ctrl"),
                        cbind.data.frame(coef=gen_1$boot[2,], region_sample=rr, region_drought="NW", bootid=1:nboot, ctrl_type="ctrl"),
                        cbind.data.frame(coef=gen_1$boot[3,], region_sample=rr, region_drought="SW", bootid=1:nboot, ctrl_type="ctrl"))
  coef_combined <- bind_rows(coef_combined, coef_tmp)
}
saveRDS(coef_combined, paste0(data_path, "/intermediate/pool_reg_results.rds"))

#-----------------------------------------------------------------------------------
#  to produce the regression results at the BA-fuel level
#-----------------------------------------------------------------------------------
### NOTE: the regressions will take ~ 1 hour. reduce the value of bootstrap rounds to save time
nboot <- 500
noctrl_var <- "+ year | month + orispl_unit | 0 | plant_ym"
ctrl_var <- " + log(sales_mwh_total1) + wind_monthly + solar_monthly + plant_temp + year | month + orispl_unit | 0 | plant_ym"

coef_combined <- NULL
for (rr in region_list){
  print(rr)
  data_region <- filter(data, region==rr)
  
  fuel_list <- unique(data_region$unit_fuel_type)
  for (jj in fuel_list){
    print(jj)
    BA_list <- unique(filter(data_region, unit_fuel_type==jj)$BA_name)
    for (kk in BA_list) {
      print(kk)
      
      pool_data_tmp <- filter(data_region, unit_fuel_type==jj, BA_name==kk) 
      
      gen_0 <- lfe::felm(formula=as.formula(paste("log(grossload_mw) ~ runoff_9_anomaly_CA +  runoff_9_anomaly_NW +  runoff_9_anomaly_SW ", noctrl_var)),  
                         data=pool_data_tmp, Nboot=nboot, weights=pool_data_tmp$monthly_gen)
      gen_1 <- lfe::felm(formula=as.formula(paste("log(grossload_mw) ~ runoff_9_anomaly_CA +  runoff_9_anomaly_NW +  runoff_9_anomaly_SW ", ctrl_var)),  
                         data=pool_data_tmp, Nboot=nboot, weights=pool_data_tmp$monthly_gen)
      
      coef_tmp <- bind_rows(cbind.data.frame(coef=gen_0$boot[1,], region_sample=rr, region_drought="CA", unit_fuel_type = jj, BA_name = kk, bootid=1:nboot, ctrl_type="no ctrl"),
                            cbind.data.frame(coef=gen_0$boot[2,], region_sample=rr, region_drought="NW", unit_fuel_type = jj, BA_name = kk, bootid=1:nboot, ctrl_type="no ctrl"),
                            cbind.data.frame(coef=gen_0$boot[3,], region_sample=rr, region_drought="SW", unit_fuel_type = jj, BA_name = kk, bootid=1:nboot, ctrl_type="no ctrl"),
                            cbind.data.frame(coef=gen_1$boot[1,], region_sample=rr, region_drought="CA", unit_fuel_type = jj, BA_name = kk, bootid=1:nboot, ctrl_type="ctrl"),
                            cbind.data.frame(coef=gen_1$boot[2,], region_sample=rr, region_drought="NW", unit_fuel_type = jj, BA_name = kk, bootid=1:nboot, ctrl_type="ctrl"),
                            cbind.data.frame(coef=gen_1$boot[3,], region_sample=rr, region_drought="SW", unit_fuel_type = jj, BA_name = kk, bootid=1:nboot, ctrl_type="ctrl"))
    
      coef_combined <- bind_rows(coef_combined, coef_tmp)
    }
  }
}

saveRDS(coef_combined, paste0(data_path, "/intermediate/BA_fuel_reg_results.rds"))

