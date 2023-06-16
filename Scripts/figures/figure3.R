library(ggplot2)
library(dplyr)   
library(MetBrewer)

data_path <- "~/Data"

lb_to_kg <- 0.4536
lb_to_kton <- 0.0004536*1e-3
#----------------------------------------------------------------------
#  to produce figure 3A in the paper 
#---------------------------------------------------------------------
unit_emis <- readRDS(paste0(data_path, "/intermediate/unit_drought_emis_historical.rds")) %>% 
                              mutate(snx_drought_emis = so2_total + nox_total,
                                     snx_total_emis = so2_mass_lbs + nox_mass_lbs) %>% ungroup() %>%
       select(lat, lon, primary_fuel_type, orispl, unitid, year, month, 
              snx_drought_emis, snx_total_emis)

plant_emis_summ <- unit_emis %>% group_by(year, orispl, primary_fuel_type, lat, lon) %>% 
  summarise(snx_drought_emis=mean(snx_drought_emis, na.rm=T), 
            snx_total_emis=mean(snx_total_emis, na.rm=T)) %>% ungroup() %>%
  mutate(percent = snx_drought_emis / snx_total_emis) %>% 
  mutate(percent=replace(percent, percent>0.4, 0.4),
         percent=replace(percent, percent< -0.1, -0.1)) ## the last two lines just for plotting ourposes

ggplot() + geom_polygon(data=states_map,aes(long, lat, group = group),fill="white",color ="black") +
  geom_point(data=filter(plant_emis_summ,  year==2001),
             aes(x=lon,y=lat,size=abs(snx_drought_emis)*lb_to_kg*1e-3, colour=percent))+
  theme_bw() +   
  theme(text=element_text(size=14),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),panel.border = element_blank(),
         axis.text = element_blank(), axis.ticks = element_blank()) + labs(size="SO2+NOx (tons)", x="", y="", shape="Fuel type",colour="") +
  coord_cartesian(xlim=c(-125, -103))  +  scale_size_continuous(trans = "sqrt", limits=c(0,700),breaks = c(10,100,500), labels=c(10,100,500)) +
  scale_colour_gradientn(colors=rev(met.brewer(name="OKeeffe1", n=14, type="continuous"))[6:14],limits=c(-0.1,0.41),labels = scales::percent) + 
  scale_shape_manual(values = c(15,16,17,18))
ggsave("Figure3A.pdf",width = 6, height = 6)

#----------------------------------------------------------------------
#  to produce figure 3C in the paper 
#---------------------------------------------------------------------
coef_bins  <- read.csv(paste0(data_path, "/intermediate/pm25_reg_results.csv"))
dist_bins <- c("<50km","50-100","100-200","200-500",">500km")
coef_bins$distance <- factor(coef_bins$distance, levels=dist_bins)
lb_kton <- 0.0004536*1e-3

coef_bins$model <- factor(coef_bins$model, 
                          levels=c("State-year FE","No met","Unweighted", "State trend", "Monitor trend"), 
                          labels=c("Main model","No met","Unweighted", "State trend", "Monitor trend"))
ggplot(filter(coef_bins, model=="Main model"), aes(x=distance, y=coef/lb_kton)) +  #, model%in%c("Monitor trend"
  geom_point(position = position_dodge(0.7), size=1.5) + geom_errorbar(aes(ymin=(coef-1.96*se)/lb_kton, ymax=(coef+1.96*se)/lb_kton),position = position_dodge(0.7), width=0, size=0.7) +
  theme_bw() + scale_y_continuous(breaks=0:3, limits = c(-0.8,3.8)) + #scale_colour_discrete(type=met.brewer(name="VanGogh1", n=6, type="discrete")) +
  geom_hline(yintercept = 0, linetype="dashed", size=0.5) + labs(y="ug PM2.5 per 1000 ton emission\n",x="") +
  theme(text = element_text(size=16), axis.text= element_text(size=18), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.border=element_blank(), axis.line= element_line(colour = "black"))
ggsave("Figure3C.pdf", width=7.5, height=3.8)


