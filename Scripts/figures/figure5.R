library(ggplot2)
library(tidyverse)  
library(MetBrewer)
library(ggrepel)

data_path <- "~/Data"

scenario_results_long <- readRDS(paste0(data_path, "/intermediate/env_damages_cmip6_scenarios.rds"))
col_map <- c("salmon","purple4","skyblue3") 

#----------------------------------------------------------------------
#  to produce figure 5A in the paper 
#---------------------------------------------------------------------
ggplot(filter(scenario_results_long, scenario=="Reference", experiment=="SSP3-7.0", year==2050), 
       aes(x=experiment, y=value/1e6, fill=name), alpha=0.8) + 
  geom_boxplot() +
  coord_flip() + scale_fill_manual(values=col_map) + 
  labs(y="Annual drought-induced damage (million dollars)",x="", fill="") + theme_bw() + 
  theme(text = element_text(size = 19),panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), axis.title =  element_text(size = 19))  + geom_hline(yintercept=0, linetype="dashed")
ggsave("Figure5A.pdf", width=8.5, height=2.4)

#----------------------------------------------------------------------
#  to produce figure 5B in the paper 
#---------------------------------------------------------------------
####### for every climate model, use SSP3-7.0 as the baseline 
ssp370_baseline <-  scenario_results_long  %>% group_by(model, scenario, name, year) %>%
  mutate(ssp370_value=sum(value*as.numeric(experiment=="SSP3-7.0"))) %>% ungroup() %>%
  mutate(diff_370=value-ssp370_value)

ggplot(filter(ssp370_baseline, scenario=="Reference", experiment%in%c("SSP2-4.5", "SSP1-2.6"), year==2050), 
       aes(x=experiment, y=diff_370/1e6, fill=name), alpha=0.8) + 
  geom_boxplot() + 
  coord_flip() + scale_fill_manual(values=col_map) + 
  labs(y="Annual drought-induced damage (million dollars)",x="", fill="") + theme_bw() + 
  theme(text = element_text(size = 19),panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), axis.title =  element_text(size = 19)) + geom_hline(yintercept=0, linetype="dashed")
ggsave("Figure5B.pdf", width=8.5, height=3.5)

#----------------------------------------------------------------------
#  to produce figure 5C in the paper 
#---------------------------------------------------------------------
scenario_results_summ <- scenario_results_long %>% 
  group_by(scenario, experiment, name, year) %>% 
  summarise(mean=mean(value, na.rm=T)/1e6, median=median(value, na.rm=T)/1e6) %>% ungroup() 

scenario_list_2050 <- c("Reference" ,"0% coal", "CCS 2050", "highRE 2050") 

ggplot(filter(scenario_results_summ, scenario%in%scenario_list_2050, experiment=="SSP3-7.0",year==2050), 
       aes(x=reorder(scenario, mean), y=mean, fill=name), alpha=0.8) + 
  geom_bar(stat="identity", alpha=0.8) + 
  coord_flip() + scale_fill_manual(values=col_map) + 
  labs(y="Annual drought-induced damage (million dollars)",x="", fill="") + theme_bw() + 
  theme(text = element_text(size = 20),panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), axis.title =  element_text(size = 20)) 
ggsave("Figure5C.pdf", width=8.5, height=3.5)


#----------------------------------------------------------------------
#  to produce figure 5D in the paper 
#---------------------------------------------------------------------
emission_highRE  <- readRDS(paste0(data_path, "/intermediate/emis_highRE_2024_2050_nerc_bystate.rds")) 
## the data contains predicted drought-induced emissions under high RE scenarios at two conditions (average years and dry year)

emis_year <- emission_highRE %>% group_by(year, state, condition) %>% 
  summarise_at(vars(gen_margin:ghg_total_20), .funs=mean, na.rm=T) %>% ungroup() %>%
  mutate(ghg100_ratio=ghg_margin_100/ghg_total_100) 

state_list <- c("WA","OR","NV","UT","MT","ID","WY","CO","CA","AZ","NM")
ggplot(filter(emis_year, state%in%state_list, condition=="Average"),
       aes(x=year, y=ghg100_ratio, colour=state)) + geom_line(size=1) +
  geom_text_repel(data=filter(emis_year, state%in%state_list, condition=="Average", year==2050),
                  aes(x=2052, label=state), size=8, direction="y",min.segment.length=3) + theme_bw() +   
  theme(text = element_text(size = 24),panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),axis.text =  element_text(size = 26),legend.position="none") +
  scale_x_continuous(breaks=seq(2026, 2050, by=8)) + scale_y_continuous(labels=scales::percent) +
  labs(x="", y="% drought-induced GHG")
ggsave("Figure5D_average.pdf", width=6.3, height=6)

ggplot(filter(emis_year, state%in%state_list, condition=="Dry year"),
       aes(x=year, y=ghg100_ratio, colour=state)) + geom_line(size=1) +
  geom_text_repel(data=filter(emis_year, state%in%state_list, condition=="Dry year", year==2050),
                  aes(x=2052, label=state), size=8, direction="y",min.segment.length=3) + theme_bw() +   
  theme(text = element_text(size = 24),panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),axis.text =  element_text(size = 26),legend.position="none") +
  scale_x_continuous(breaks=seq(2026, 2050, by=8)) + scale_y_continuous(labels=scales::percent) +
  labs(x="", y="% drought-induced GHG")
ggsave("Figure5D_dry_year.pdf", width=6.3, height=6)




