library(ggplot2)
library(dplyr)  
library(tidyr)
library(MetBrewer)

data_path <- "~/Data"

#----------------------------------------------------------------------
#  to produce figure 4A in the paper 
#---------------------------------------------------------------------
drought_damages <- readRDS(paste0(data_path, "/intermediate/historical_env_damages_year_month.rds"))

col_map <- c("salmon","purple4","skyblue3")  
ggplot(filter(drought_damages, model=="Empirical, 100km"),aes(x=date,y=value/1e6, fill=name)) + geom_area(alpha=0.85) +
  labs(x="", y="Monetized damage (million $)") + theme_bw() +
  theme(text = element_text(size = 22),panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),axis.text.x =  element_text(size = 18)) +  # facet_wrap(~model, ncol=2) +
  theme(legend.title=element_blank(),legend.text = element_text(size = 18, margin = margin(l = 10, unit = "pt")),legend.box.margin=margin(10,10,10,10),legend.key.size = unit(1.2, "lines"),legend.spacing.y = unit(0.3, 'cm')) +
  scale_fill_manual(values=col_map) + scale_x_continuous(breaks=as.Date(c("2001-01-01","2006-01-01","2011-01-01","2016-01-01","2021-01-01")), labels=seq(2001,2021,5)) +
  geom_vline(aes(xintercept=as.Date("2012-01-01")), linetype="dashed", size=0.7) + geom_vline(aes(xintercept=as.Date("2016-12-31")), linetype="dashed", size=0.7) + 
  scale_y_continuous(breaks=c(-250, 0, 250, 500, 750), limits=c(-291, 801)) + guides(fill = guide_legend(byrow = TRUE))
ggsave("Figure4A.pdf", width=10, height=5)

#----------------------------------------------------------------------
#  to produce figure 4B in the paper 
#---------------------------------------------------------------------
raw_data <- readRDS(paste0(data_path,"/input/runoff_generation_month_final.rds")) 

month_data <- filter(raw_data, year>2000, year<2022) %>% group_by(year, month) %>%
  summarise(monthly_gen=sum(grossload_mw, na.rm=T), monthly_so2=sum(so2_mass_lbs, na.rm=T),
            monthly_co2=sum(co2_mass_tons, na.rm=T), monthly_nox=mean(nox_mass_lbs, na.rm=T)) %>% ungroup() %>%
  mutate(co2_ef=monthly_co2/monthly_gen, so2_ef=monthly_so2/monthly_gen, nox_ef=monthly_nox/monthly_gen)

co2_ef_2021 <- mean(filter(month_data, year==2021)$co2_ef, na.rm=T)
nox_ef_2021 <- mean(filter(month_data, year==2021)$nox_ef, na.rm=T)
so2_ef_2021 <- mean(filter(month_data, year==2021)$so2_ef, na.rm=T)

month_data_1 <-  month_data %>% 
  mutate(co2_normalize=co2_ef/co2_ef_2021,
         nox_normalize=nox_ef/nox_ef_2021,
         so2_normalize=so2_ef/so2_ef_2021)

month_data_1 <- month_data_1 %>% select(year, month, co2_normalize:so2_normalize) %>% pivot_longer(cols = co2_normalize:so2_normalize) %>% 
  mutate(date=as.Date(paste(year,month,"15",sep="-")))

month_data_1$name <- factor(month_data_1$name, levels=c("co2_normalize","so2_normalize","nox_normalize"), labels=c("CO2","SO2","NOx"))
col_map <- met.brewer(name="Egypt", n=3, type="discrete")

## the following code plots SO2 and NOx changes
ggplot(filter(month_data_1, name!="CO2"), aes(x=date, y=value, colour=name)) + geom_line(size=1.2) + geom_hline(aes(yintercept=1), linetype="dashed") +
  labs(x="", y="") + theme_bw() +
  theme(text = element_text(size = 22),panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),axis.text.x =  element_text(size = 20)) +  # facet_wrap(~model, ncol=2) +
  theme(legend.title=element_blank(),legend.text = element_text(size = 18, margin = margin(l = 10, unit = "pt")),legend.box.margin=margin(10,10,10,10),legend.key.size = unit(1.2, "lines")) +
  scale_colour_manual(values=col_map[2:3]) + scale_x_continuous(breaks=as.Date(c("2001-01-01","2006-01-01","2011-01-01","2016-01-01","2021-01-01")), labels=seq(2001,2021,5)) + scale_y_continuous(breaks=c(1,4,7,10))
ggsave("Figure4B.pdf", width=9, height=3.5)

#----------------------------------------------------------------------
#  to produce figure 4C in the paper (focusing on 2012-2016, and impacts from CA plants and the CA drought)
#---------------------------------------------------------------------
mort_all <-  readRDS(paste0(data_path, "/intermediate/historical_pm25_mortality.rds"))
mort_inCA <- filter(mort_all, state=="California", model=="Empirical, 100km")   ##counting for all impacts on CA
mort_inCA$type <- "In CA"

mort_droughtCA <- readRDS(paste0(data_path, "/intermediate/historical_pm25_mortality_CAdrought.rds")) %>%
  filter(state!="California", model=="Empirical, 100km") ##counting for impacts due to CA drought (excluding CA to avoid double counting)
mort_droughtCA$type <- "CA drought"
mort_CA_summ <- bind_rows(mort_inCA, mort_droughtCA) %>%
  group_by(year, month, type) %>% 
  summarise(mort_dery1=sum(mort_dery1)) %>% ungroup()
  
drought_emission <- readRDS(paste0(data_path, "/intermediate/unit_drought_emis_historical.rds")) 

vsl_us <- 10.95*1e6
inflate_20_19 <- 1.00953 ### gdp_deflator_index from EIA [calculated as = index_2020 / index_2019]
inflate_20_16 <- 1.07231 ### gdp_deflator_index from EIA [calculated as = index_2020 / index_2016]
vsl_adjust <- vsl_us*inflate_20_19
scc <- 117 ### from the latest EPA report (2.5% discount rate)
scc_high <- 193
sc_ch4 <- 1257
ch4_leakage <- 0.023
lb_to_kg <- 0.453592
us_to_metric_ton <- lb_to_kg*2 

#### calculate the CH4 leakage ##########
gas_consumption <- readRDS(paste0(data_path, "/input/fuel_gas_consumption_2001_2021.rds")) %>% 
  filter(year==2021)
gas_rate_state <- gas_consumption %>% distinct(state, state_median) 
drought_emission  <- left_join(drought_emission, gas_consumption[,c("ton_gas_mwh","orispl")], by=c("orispl"))
drought_emission  <- left_join(drought_emission, gas_rate_state, by=c("state"))
drought_emission  <- drought_emission %>% 
  mutate(ton_gas_mwh=replace(ton_gas_mwh, (is.na(ton_gas_mwh) & unit_fuel_type%in%c("NG-GT","NG-CT","NG-ST","NG")), 
                             state_median[(is.na(ton_gas_mwh) & unit_fuel_type%in%c("NG-GT","NG-CT","NG-ST","NG"))])) %>%
  mutate(ch4_used=gen_total*ton_gas_mwh, ch4_used_CA=gen_CA*ton_gas_mwh)

emis_summ_CA1 <- filter(drought_emission, region_sample=="CA") %>%  ##counting for emissions from CA plants
  group_by(year, month) %>% 
  summarise(co2=sum(co2_total, na.rm=T), ch4_leak=sum(ch4_used*ch4_leakage, na.rm=T)) %>% ungroup() %>%
  mutate(type = "In CA")

emis_summ_CA2 <- filter(drought_emission, region_sample!="CA") %>% ##counting for emissionsdue to CA droughts (but from non-CA plants)
  group_by(year, month) %>% 
  summarise(co2=sum(co2_CA, na.rm=T), ch4_leak=sum(ch4_used_CA*ch4_leakage, na.rm = T)) %>% ungroup() %>%
  mutate(type = "CA drought") 

emis_CA <- bind_rows(emis_summ_CA1,emis_summ_CA2) 
  
drought_results_CA <- full_join(mort_CA_summ, emis_CA, by=c("year","month","type")) %>% 
  select(year, month, type, mort_dery1, co2, ch4_leak) %>% 
  mutate(mort_cost=mort_dery1*vsl_adjust, 
         ch4_cost=ch4_leak*us_to_metric_ton*sc_ch4, 
         co2_cost=co2*us_to_metric_ton*scc, 
         co2_extra=co2*us_to_metric_ton*(scc_high-scc)) %>% select(-mort_dery1, -co2, -ch4_leak) %>%
  pivot_longer(cols=mort_cost:co2_extra)
drought_results_CA$date <- as.Date(paste(drought_results_CA$year, drought_results_CA$month, "15", sep="-"))

drought_results_CA$name <- factor(drought_results_CA$name, levels=c("co2_cost","co2_extra","ch4_cost","mort_cost"), 
                                  labels=c("CO2 damage","CO2 damage extra","CH4 leakage","PM2.5 damage"))

drought_2012_2016 <- filter(drought_results_CA, date>"2011-12-31", date<"2016-12-31")  %>%
  group_by(name) %>% summarise(value=sum(value, na.rm=T)) %>%
  mutate(study="This work") %>% ungroup() 

### compare with prior estimates
drought_2012_2016 <- bind_rows(drought_2012_2016,
                               data.frame(name="Economic loss", 
                                          value=inflate_20_16*c(3.814*1e9, 2.45*1e9, 1.913*1e9), 
                                          study=c("demand change","Gleick 2017", "Kern et al., 2020"))) 

drought_2012_2016$study <- factor(drought_2012_2016$study, 
                                  levels=c("This work", "Gleick 2017", "Kern et al., 2020","demand change"))  
drought_2012_2016$name <- factor(drought_2012_2016$name, 
                                 levels=c("CO2 damage extra","CO2 damage","CH4 leakage", "PM2.5 damage", "Economic loss")) 

ggplot(drought_2012_2016, aes(x=study, y=value/1e9, fill=name))  + 
  geom_bar(stat = "identity", aes(alpha=name))   +
  scale_alpha_manual(values=c(0.5,0.9,0.9,0.9,0.9)) +                                                                            
  scale_fill_manual(values=c("salmon","salmon","purple4","skyblue3","grey40")) + labs(x="", y="Monetized damage (billion $)") + theme_bw() +
  theme(text = element_text(size = 22),panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),axis.text.x =  element_text(size = 18)) + 
  theme(legend.title=element_blank(),legend.text = element_text(size = 18, margin = margin(l = 10, unit = "pt")),legend.box.margin=margin(10,10,10,10),legend.key.size = unit(1, "lines"),legend.spacing.y = unit(1, 'cm'))+
  guides(fill = guide_legend(byrow = TRUE)) + scale_y_continuous(breaks=c(0, 2, 4, 6, 8), limits=c(0, 8.01)) 
ggsave("Figure4C.pdf", width=10.5, height=5.2) 

