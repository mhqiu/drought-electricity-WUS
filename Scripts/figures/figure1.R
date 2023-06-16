library(ggplot2)
library(dplyr)   
library(fixest)
library(MetBrewer)
library(zoo)
library(sf); library(sfheaders)

data_path <- "~/Data"

#----------------------------------------------------------------------
#  to produce figure 1 in the paper 
#---------------------------------------------------------------------
### Figure 1A
runoff_west <- readRDS(paste0(data_path,"/input/runoff_month_1980_2021.rds"))
runoff_west$runoff_avg <- runoff_west$runoff_9_anomaly
runoff_west$date <- as.Date(paste(runoff_west$year, runoff_west$month, "15", sep="-"))
runoff_west$drought <- as.numeric(runoff_west$runoff_avg<0)
## just for visualization purpose
runoff_west_aug <- runoff_west %>% arrange(date) %>% mutate(lead=lead(runoff_avg)) %>%
  mutate(sign_change = runoff_avg/lead(runoff_avg) < 0) %>% 
  filter(sign_change == TRUE) %>% 
  mutate(date = date + 30* abs(runoff_avg)/(abs(lead) + abs(runoff_avg)), runoff_avg = 0, drought=0) %>%
  select(-sign_change, -lead)
runoff_west_aug1 <- runoff_west_aug %>% mutate(drought=1) 
runoff_west_1 <- bind_rows(runoff_west_aug, runoff_west_aug1, runoff_west)

col_map <- c("steelblue4","sienna1")

ggplot(filter(runoff_west_1), aes(x=date, y= runoff_avg)) + 
   geom_area(aes(fill=factor(drought)), alpha=0.8) + 
   geom_smooth(method = lm, colour="black", alpha=0.5) + 
   geom_hline(yintercept=0, linetype="dashed", size=0.8) +
  labs(x="", y="Runoff anomalies") + theme_bw() +
  theme(text = element_text(size = 26),panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),axis.text.x =  element_text(size = 24)) +
  theme(legend.title=element_blank(),legend.text = element_text(size = 18, margin = margin(l = 10, unit = "pt")),legend.box.margin=margin(10,10,10,10),legend.key.size = unit(1.2, "lines")) +
  scale_fill_manual(values=col_map) +  scale_y_continuous(labels = scales::percent) + guides(fill="none")
ggsave("Figure1A.pdf", width=9, height=4.5)

#######  Figure 1B
monthly_covariate <- readRDS(paste0(data_path, "/input/region_monthly_covariates.rds"))

west_data <- filter(monthly_covariate, !is.na(runoff_9_anomaly), !is.na(hydro_monthly)) %>% 
  group_by(year, month) %>% 
  summarise(hydro_region=sum(hydro_monthly, na.rm=T)) %>%  ungroup()
west_data$year <- as.numeric(west_data$year)

runoff_west <- readRDS(paste0(data_path,"/input/runoff_month_1980_2021.rds"))
runoff_west$runoff_region <- runoff_west$runoff_9_anomaly
west_data <- inner_join(west_data, runoff_west[,c("year","month","runoff_region")], by=c("year","month"))

west_data_1 <- west_data  %>% mutate(hydro_residual=feols(hydro_region~ year | month , data=.)$residuals,
                                     runoff_region_residual=feols(runoff_region ~ year | month , data=.)$residuals) 

west_data_1 <- west_data_1 %>% group_by(month) %>% mutate(hydro_month_mean=mean(hydro_region)) %>% ungroup()

ggplot(west_data_1, aes(x=runoff_region_residual, y=hydro_residual/hydro_month_mean)) + geom_point(size=1.7) + geom_smooth(method="lm", colour="blue") +
  scale_y_continuous(labels = scales::percent) + theme_bw() +
  theme(text = element_text(size = 25),panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),axis.text.x =  element_text(size = 23)) +
  labs(x="Runoff anomalies", y="Hydro generation") + scale_y_continuous(labels = scales::percent) + 
  scale_x_continuous(labels = scales::percent, limits=c(-0.5,0.51), breaks=seq(-0.5,0.5,by=0.25))
ggsave("Figure1B.pdf", width=8, height=4.2)


#######  figure 1C: location of the AMPD plants  ########
sf_use_s2(FALSE)
nerc_shp <- st_read(paste0(data_path, "/input/egrid2020_subregions/eGRID2020_subregions.shp")) %>%
             mutate(id_new = ZipSubregi) %>%
             mutate(id_new=replace(id_new, id_new %in%c("NWPP","RMPA"), "NW"))

nerc_shp_nw <- filter(nerc_shp, id_new=="NW") %>% group_by(id_new)  %>% 
  summarise(geometry = sf::st_union(geometry)) %>% ungroup()
nerc_west <- bind_rows(nerc_shp_nw, filter(nerc_shp, id_new%in%c("CAMX","AZNM"))) %>% sfheaders::sf_remove_holes()

states_map <- map_data("state")

raw_data <- readRDS(paste0(data_path,"/input/runoff_generation_month_final.rds"))
plant_data <- raw_data %>% group_by(orispl, unitid, primary_fuel_type, lon, lat) %>% 
  summarise(grossload_gwh=mean(grossload_mw,na.rm=T)*1e-3) %>% ungroup() %>%
  group_by(orispl, primary_fuel_type, lon, lat) %>% 
  summarise(grossload_gwh=sum(grossload_gwh,na.rm=T)) %>% ungroup() %>% 
  mutate(fuel_type="Other") %>%
  mutate(fuel_type=replace(fuel_type, primary_fuel_type%in%c("NG"), "Gas"),
        fuel_type=replace(fuel_type, primary_fuel_type%in%c("BIT","SUB","RC"), "Coal"))

ggplot() + geom_polygon(data=filter(states_map, !region%in%c("texas","oklahoma","kansas","nebraska", "north dakota","south dakota")),aes(long, lat, group = group),fill="white",color ="black",size=0.3) + 
  geom_sf(data=nerc_west, aes(fill=id_new),color ="black",size=0.5,alpha=0.3) +#scale_fill_manual(values=iso_palette,guide='none') + #scale_fill_hue(h=c(30,170),guide = 'none')+ #+ scale_fill_manual(values=iso_palette,guide='none') + #fill="grey90",
  geom_point(data=plant_data,aes(x=lon,y=lat,colour=fuel_type, size=grossload_gwh))+
  theme_bw() +   theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(),panel.border = element_blank()) +
  theme(text=element_text(size=12),legend.text = element_text(size = 12),axis.text.x = element_blank(), axis.text.y = element_blank(),axis.ticks = element_blank()) + 
  scale_size_continuous(limits=c(0,1500),range = c(0.5, 4), breaks=c(10,100,1000)) +
  labs(title="",x="",y="",size="Generation (GWh)", colour="") +
  guides(shape = guide_legend(override.aes = list(size=3),order = 1), size = guide_legend(override.aes = list(title="Generation (GWh)",color="black"), order = 2), fill = 'none') +
  coord_sf(xlim=c(-125,-102), ylim=c(25,55)) +
  scale_fill_discrete(type=met.brewer(name="Egypt", n=3, type="discrete")) +
  scale_colour_discrete(type=c("grey25","brown4","blue")) 
ggsave("Figure1C.pdf",width=5.5,height=5)

#######  figure 1c: the fuel mix of pie charts ########
monthly_covariate <- readRDS(paste0(data_path, "/input/region_monthly_covariates.rds"))

gen_region <- filter(monthly_covariate, year>2000) %>% group_by(region,year) %>% 
  summarise_at(2:7, .funs=sum, na.rm=T) %>% ungroup() %>% 
  group_by(region) %>% summarise_at(2:7, .funs=mean, na.rm=T) %>%
  mutate(renew=wind_monthly+solar_monthly, 
         other=total_monthly-hydro_monthly-wind_monthly-solar_monthly-nuclear_monthly-fossil_monthly) %>% ungroup()
gen_region_long <- gen_region %>% select(region, hydro_monthly, renew, fossil_monthly, nuclear_monthly ,other) %>% 
  pivot_longer(cols=hydro_monthly:other) 

gen_region_long$name <- factor(gen_region_long$name, 
                               levels=rev(c("hydro_monthly","fossil_monthly","renew","nuclear_monthly","other")),
                               labels=rev(c("Hydro","Fossil","Renewable","Nuclear","Other")))
gen_region_long <- gen_region_long %>% group_by(region) %>% mutate(total=sum(value, na.rm=T)) 
col_map <- rev(c("steelblue3","grey35","green4","salmon","mediumpurple1"))

ggplot(gen_region_long, aes(x=total/2,y=value*1e-6, fill=name, width = total)) + 
  geom_bar(position="fill", stat="identity", alpha=0.85) + 
  coord_polar("y") + facet_wrap(~region) +
  labs(x="", y="Generation (TWh)") + theme_bw() +
  theme(text = element_text(size = 20),panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),axis.text.x =  element_blank(), axis.text.y =  element_blank()) +
  theme(legend.title=element_blank(),legend.text = element_text(size = 18, margin = margin(l = 2, unit = "pt")),legend.box.margin=margin(10,10,10,10),legend.key.size = unit(1.2, "lines"),legend.spacing.y = unit(0.1, 'cm')) +
  scale_fill_manual(values=col_map)  +  guides(fill = guide_legend(byrow = TRUE))
ggsave("Figure1C_pie.pdf", width=10, height=6)
