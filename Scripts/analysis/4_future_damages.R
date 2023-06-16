library(tidyverse)

data_path <- "~/Data"

#----------------------------------------------------------------------------------
#  to produce the future damage results (used to plot figure 5)
#----------------------------------------------------------------------------------
### load the drought-induced emissions from CMIP6 scenarios
emis_scenario <- readRDS(paste0(data_path, "/input/monthly_emis_cmip6_scenarios.rds")) 

# calculate the CH4 usage and leakage
gas_consumption <- readRDS(paste0(data_path, "/input/fuel_gas_consumption_2001_2021.rds")) %>% 
  filter(year==2021)
gas_rate_state <- gas_consumption %>% distinct(state, state_median) 
emis_scenario1  <- left_join(emis_scenario, gas_consumption[,c("ton_gas_mwh","orispl")], by=c("orispl"))
emis_scenario1  <- left_join(emis_scenario1, gas_rate_state, by=c("state")) %>% 
  mutate(ton_gas_mwh=replace(ton_gas_mwh, (is.na(ton_gas_mwh) & unit_fuel_type%in%c("NG-GT","NG-CT","NG-ST","NG")), 
                             state_median[(is.na(ton_gas_mwh) & unit_fuel_type%in%c("NG-GT","NG-CT","NG-ST","NG"))])) %>%
  mutate(ch4_used=gen_total*ton_gas_mwh)

emis_summ <- emis_scenario1 %>% group_by(scenario,  model, experiment) %>% 
  summarise(co2_total=sum(co2_total, na.rm=T),
            ch4_used=sum(ch4_used, na.rm=T)) %>% ungroup()

emis_2035 <- filter(emis_summ, scenario%in%c("Reference","50% coal","CCS 2035","highRE-2034-import")) %>%
  mutate(year=2035)
emis_2050 <- filter(emis_summ, scenario%in%c("Reference","0% coal","CCS 2050","highRE-2050-import")) %>% 
  mutate(year=2050)

###load the mortality results from future drough emissions and merge 
mortality <- readRDS(paste0(data_path, "/intermediate/mortality_scenarios_cmip6.rds"))

scenario_results <- left_join(bind_rows(emis_2035, emis_2050), 
                              mortality, by=c("scenario","model","experiment","year")) 

vsl_us <- 10.95*1e6
inflate_20_19 <- 1.00953 ### gdp_deflator_index from EIA [calculated as = index_2020 / index_2019]
vsl_adjust <- vsl_us*inflate_20_19
lb_to_kg <- 0.453592
us_to_metric_ton <- lb_to_kg*2 
us_gdp_2050_2020 <- 1.328
us_gdp_2035_2020 <- 1.194
discount_2035 <- (1-0.025)^15
discount_2050 <- (1-0.025)^30
ch4_leakage <- 0.023

### scale the VSL and SC-GHG for future accounting
future_metric <- data.frame(year=c(2035,2050), scc=c(158,205), sc_ch4=c(2313,3547), 
                            vsl=vsl_adjust*c(us_gdp_2035_2020, us_gdp_2050_2020),
                            discount_2020=c(discount_2035, discount_2050))

scenario_results$scenario <- factor(scenario_results$scenario, 
                                    levels=c("0% coal","50% coal","CCS 2035", "CCS 2050","highRE-2034-import",
                                             "highRE-2050-import", "Reference"), 
                                    labels=c("0% coal","50% coal","CCS 2035", "CCS 2050","highRE 2035",
                                             "highRE 2050", "Reference"))
scenario_results$experiment <- factor(scenario_results$experiment, levels=c("ssp126","ssp245","ssp370","ssp585"), 
                                      labels = c("SSP1-2.6","SSP2-4.5","SSP3-7.0","SSP5-8.5")) 

scenario_results1 <- full_join(scenario_results, future_metric, by="year")

scenario_results_long <- scenario_results1 %>% 
  mutate(co2_cost=co2_total*us_to_metric_ton*scc*discount_2020, 
         ch4_cost=ch4_used*ch4_leakage*us_to_metric_ton*sc_ch4*discount_2020, 
         mort_cost=mort*vsl*discount_2020) %>% 
  select(scenario, model, experiment, year, co2_cost, ch4_cost, mort_cost) %>%
  pivot_longer(cols=co2_cost:mort_cost) 
scenario_results_long$name <- factor(scenario_results_long$name,  levels=c("co2_cost","ch4_cost","mort_cost"), 
                                     labels=c("CO2 damage","CH4 leakage","PM2.5 damage"))
scenario_results_long$value <- 12*scenario_results_long$value  ### translate from monthly damages to annual damages (because emissions and mortality corresponds to monthly data)
saveRDS(scenario_results_long, paste0(data_path, "/intermediate/env_damages_cmip6_scenarios.rds"))
