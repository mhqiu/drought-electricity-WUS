library(tidyverse)

data_path <- "~/Data"

#----------------------------------------------------------------------------------
#  to calculate the historical drought-related emissions (part of Figure 3)
#----------------------------------------------------------------------------------
## load the coefs from pooled reg and BA-fuel regressions
# use the "no ctrl" coefs for self-effects (i.e. region_drought == region_sample),
# use the "ctrl" coefs for transboundary effects (i.e. region_drought != region_sample)

pooled_coef <- readRDS(paste0(data_path, "/intermediate/pool_reg_results.rds"))
pooled_coef_self <-  filter(pooled_coef, region_drought == region_sample, ctrl_type=="no ctrl")
pooled_coef_other <-  filter(pooled_coef, region_drought != region_sample, ctrl_type=="ctrl")

pooled_coef_combined <- bind_rows(pooled_coef_self, pooled_coef_other) %>% 
  rename(coef_pooled=coef) %>% select(-ctrl_type) %>%
  pivot_wider(names_prefix = "coef_pooled_", names_from = region_drought, values_from = coef_pooled)

bafuel_coef <- readRDS(paste0(data_path, "/intermediate/BA_fuel_reg_results.rds")) 
bafuel_coef_self <-  filter(bafuel_coef, region_drought == region_sample, ctrl_type=="no ctrl")
bafuel_coef_other <-  filter(bafuel_coef, region_drought != region_sample, ctrl_type=="ctrl")

bafuel_coef_combined <- bind_rows(bafuel_coef_self, bafuel_coef_other) %>% 
  select(-ctrl_type) %>%
  pivot_wider(names_prefix = "coef_", names_from = region_drought, values_from = coef)

coef_combined <- left_join(bafuel_coef_combined, pooled_coef_combined) %>%
  mutate(bafuel=paste(BA_name, unit_fuel_type, sep="_"))

### for 2 BA-fuel pairs for (11 EGUs), we replace the BA-fuel level EGUs with the pooled coefs (otherwise the estimates are non-realistic)
replace_bafuel <- c("nevada power company_RFO","puget sound energy_NG")

coef_combined <- coef_combined %>% 
  mutate(coef_CA=replace(coef_CA, bafuel%in%replace_bafuel, coef_pooled_CA[bafuel%in%replace_bafuel]),
         coef_NW=replace(coef_NW, bafuel%in%replace_bafuel, coef_pooled_NW[bafuel%in%replace_bafuel]),
         coef_SW=replace(coef_SW, bafuel%in%replace_bafuel, coef_pooled_SW[bafuel%in%replace_bafuel]))

## calculate the median of the estimated coefs from the 500 bootstrap runs
coef_combined <- coef_combined %>% group_by(region_sample, unit_fuel_type, BA_name) %>%
  summarise_at(c("coef_CA","coef_SW","coef_NW"), .funs = median) %>% ungroup()

### merge unit-level data with coefs and calculate the drought-induced generation and emission
raw_data <- readRDS(paste0(data_path,"/input/runoff_generation_month_final.rds")) %>%
  filter(n_gen_positive >=1) %>%
  rename(region_sample=region)

data <- left_join(raw_data, coef_combined) %>% filter(!is.na(coef_CA))

drought_emission  <- data %>% 
      select(orispl, unitid, lat, lon, primary_fuel_type, unit_fuel_type, region_sample, state,  
             year, month,
             grossload_mw, co2_mass_tons, so2_mass_lbs, nox_mass_lbs, 
             runoff_9_anomaly_CA, runoff_9_anomaly_NW, runoff_9_anomaly_SW,
             coef_CA, coef_NW, coef_SW) %>%
                     mutate(grossload_mw = ifelse(is.na(grossload_mw), 0, grossload_mw),
                            co2_mass_tons = ifelse(is.na(co2_mass_tons), 0, co2_mass_tons),
                            so2_mass_lbs = ifelse(is.na(so2_mass_lbs), 0, so2_mass_lbs),
                            nox_mass_lbs = ifelse(is.na(nox_mass_lbs), 0, nox_mass_lbs)) %>%
                     mutate(gen_CA=(exp(coef_CA*runoff_9_anomaly_CA)-1)*grossload_mw, 
                            gen_NW=(exp(coef_NW*runoff_9_anomaly_NW)-1)*grossload_mw, 
                            gen_SW=(exp(coef_SW*runoff_9_anomaly_SW)-1)*grossload_mw,
                            so2_CA=(exp(coef_CA*runoff_9_anomaly_CA)-1)*so2_mass_lbs, 
                            so2_NW=(exp(coef_NW*runoff_9_anomaly_NW)-1)*so2_mass_lbs, 
                            so2_SW=(exp(coef_SW*runoff_9_anomaly_SW)-1)*so2_mass_lbs,
                            nox_CA=(exp(coef_CA*runoff_9_anomaly_CA)-1)*nox_mass_lbs, 
                            nox_NW=(exp(coef_NW*runoff_9_anomaly_NW)-1)*nox_mass_lbs, 
                            nox_SW=(exp(coef_SW*runoff_9_anomaly_SW)-1)*nox_mass_lbs,
                            co2_CA=(exp(coef_CA*runoff_9_anomaly_CA)-1)*co2_mass_tons, 
                            co2_NW=(exp(coef_NW*runoff_9_anomaly_NW)-1)*co2_mass_tons, 
                            co2_SW=(exp(coef_SW*runoff_9_anomaly_SW)-1)*co2_mass_tons) %>%
                     mutate(gen_total=gen_CA+gen_NW+gen_SW, 
                            co2_total=co2_CA+co2_NW+co2_SW,
                            so2_total=so2_CA+so2_NW+so2_SW, 
                            nox_total=nox_CA+nox_NW+nox_SW) %>% ungroup()

saveRDS(drought_emission, paste0(data_path, "/intermediate/unit_drought_emis_historical.rds"))


#----------------------------------------------------------------------------------
#  to calculate the environmental damages of drought-induced emissions (Figure 4)
#----------------------------------------------------------------------------------
# step.1 calculate the gas usage of each plant and used to calculate CH4 leakage 
gas_consumption <- readRDS(paste0(data_path, "/input/fuel_gas_consumption_2001_2021.rds")) %>% 
               filter(year==2021)
gas_rate_state <- gas_consumption %>% distinct(state, state_median) 
drought_emission  <- left_join(drought_emission, gas_consumption[,c("ton_gas_mwh","orispl")], by=c("orispl"))
drought_emission  <- left_join(drought_emission, gas_rate_state, by=c("state"))
drought_emission  <- drought_emission %>% 
  mutate(ton_gas_mwh=replace(ton_gas_mwh, (is.na(ton_gas_mwh) & unit_fuel_type%in%c("NG-GT","NG-CT","NG-ST","NG")), 
         state_median[(is.na(ton_gas_mwh) & unit_fuel_type%in%c("NG-GT","NG-CT","NG-ST","NG"))])) %>%
  mutate(ch4_used=gen_total*ton_gas_mwh)

emis_summ <- drought_emission %>% group_by(year, month) %>% 
  summarise_at(c("co2_total", "ch4_used"), .funs = sum, na.rm=T) %>% ungroup()

# step.2 load the PM25-related mortality results and merge with the main results
mortality <-   readRDS(paste0(data_path, "/intermediate/historical_pm25_mortality.rds")) %>%
        filter(model=="Empirical, 100km") %>%
           group_by(year, month) %>% 
              summarise(mort_dery1=sum(mort_dery1)) %>% ungroup()
drought_results <- full_join(mortality, emis_summ, by=c("year","month")) 

# step.3  monetize the impacts using SCC, VSL
vsl_us <- 10.95*1e6
inflate_20_19 <- 1.00953 ### gdp_deflator_index from EIA [calculated as = index_2020 / index_2019]
vsl_adjust <- vsl_us*inflate_20_19
scc <- 117 ### from the latest EPA report (2.5% discount rate)
scc_high <- 193
sc_ch4 <- 1257
ch4_leakage <- 0.023
lb_to_kg <- 0.453592
us_to_metric_ton <- lb_to_kg*2 

drought_results_long <- drought_results %>% 
  select(year, month, model, mort_dery1, co2_total, ch4_used) %>% 
  mutate(mort_cost=mort_dery1*vsl_adjust, 
         co2_cost=co2_total*us_to_metric_ton*scc, 
         ch4_cost=ch4_used*sc_ch4*us_to_metric_ton*ch4_leakage) %>% 
  select(-mort_dery1, -co2_total, -ch4_used) %>%
  pivot_longer(cols=mort_cost:ch4_cost)

drought_results_long$date <- as.Date(paste(drought_results_long$year, drought_results_long$month, "15", sep="-"))
drought_results_long$name <- factor(drought_results_long$name, levels=c("co2_cost","ch4_cost","mort_cost"), labels=c("CO2 damage","CH4 leakage","PM2.5 damage"))
saveRDS(drought_results_long, paste0(data_path, "/intermediate/historical_env_damages_year_month.rds"))

