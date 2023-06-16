library(ggplot2)
library(dplyr)   

data_path <- "~/Data"

#----------------------------------------------------------------------
#  to produce figure 2 in the paper 
#---------------------------------------------------------------------
pool_coef <- readRDS(paste0(data_path, "/intermediate/pool_reg_results.rds"))

###### using no ctrl for self region, and ctrl for non self-region##
pool_coef1 <- bind_rows(filter(pool_coef, region_drought==region_sample, ctrl_type=="no ctrl"),
                        filter(pool_coef, region_drought!=region_sample, ctrl_type=="ctrl")) %>% select(-ctrl_type)

monthly_covariate <- readRDS(paste0(data_path, "/input/region_monthly_covariates.rds")) 

## Panels 2A - 2C
west_runoff <- filter(monthly_covariate, year>2000) %>% select(year, month, region, runoff_9_anomaly)
coef_combine_month <- full_join(pool_coef1, west_runoff, by=c("region_drought"="region"))
gen_predict_month <- coef_combine_month  %>% 
  group_by(region_sample, region_drought, bootid, year, month) %>%
  summarise(drought_gen=exp(coef*runoff_9_anomaly)-1) %>% ungroup()  %>% 
  group_by(region_sample, bootid, year, month) %>%
  summarise(total_effect=sum(drought_gen), 
            self_effect=sum(drought_gen*as.numeric(region_sample==region_drought)))  %>% ungroup()

gen_predict_summ <- pivot_longer(gen_predict_month, cols=total_effect:self_effect) %>% 
                          group_by(region_sample, name, year, month) %>% 
                          summarise(median=median(value), ci_025=quantile(value, 0.025), ci_975=quantile(value, 0.975)) %>% 
                          ungroup() %>% group_by(region_sample, name) %>% 
                          arrange(desc(median)) %>% mutate(index=1:n()) %>% ungroup()

####### repeat the following for the other two regions
ggplot(filter(gen_predict_summ, region_sample=="CA"), aes(x=index, colour=name, fill=name)) + 
  geom_line(aes(y=median), size=1.3) +
  geom_ribbon(aes(ymin=ci_025, ymax=ci_975), colour=NA, alpha=0.5) +
  facet_wrap(~region_sample, scale="free") + theme_bw() +
  theme(text = element_text(size=22), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.border=element_blank(), axis.line= element_line(colour = "black"),
        axis.text.x=element_blank(), axis.ticks.x=element_blank(), panel.spacing = unit(3.5, "lines")) +
  labs(x="", y="Fossil generation", colour="") + scale_y_continuous(labels = scales::percent) + guides(fill="none") +
  geom_hline(yintercept = 0, linetype="dashed") +
  scale_colour_discrete(type=c("grey20","darkorange")) +
  scale_fill_discrete(type=c("grey20","darkorange"))
ggsave("Figure2A.pdf", width=6, height=4)

## Panels 2D - 2F: code below plot CA; change the region name to NW and SW to produced panel B and C
runoff_quantile <- filter(monthly_covariate, year>2000) %>% group_by(region) %>% 
  summarise(runoff9_5=quantile(runoff_9_anomaly, 0.05, na.rm=T), 
            runoff9_95=quantile(runoff_9_anomaly, 0.95, na.rm=T)) %>% ungroup() %>%
  rename(region_drought=region)

pool_coef_summ <- left_join(pool_coef1, runoff_quantile)

gen_predict_df <- pool_coef_summ %>%
  mutate(drought_gen=exp(coef*(runoff9_5 - runoff9_95))-1)  %>% 
  group_by(region_sample, region_drought) %>% 
  summarise(median=median(drought_gen), ci_low=quantile(drought_gen, 0.025), ci_high=quantile(drought_gen, 0.975)) %>% ungroup()

gen_predict_df1 <- filter(gen_predict_df, region_sample=="CA")
ggplot() +
  geom_point(data=filter(gen_predict_df1, region_drought==region_sample), 
             aes(x= region_drought, y=median), colour="grey20", position=position_dodge(width=0.3),size=2.4) +
  geom_errorbar(data=filter(gen_predict_df1, region_drought==region_sample), 
                aes(x= region_drought, y=median, ymax=ci_high, ymin=ci_low), colour="grey20", width=0, position=position_dodge(width=0.3),size=1.1) +
  geom_point(data=filter(gen_predict_df1, region_drought!=region_sample), 
             aes(x= region_drought, y=median), colour="darkorange", position=position_dodge(width=0.3),size=2.4) +
  geom_errorbar(data=filter(gen_predict_df1 ,region_drought!=region_sample), 
                aes(x= region_drought, y=median, ymax=ci_high, ymin=ci_low), colour="darkorange", width=0, position=position_dodge(width=0.3),size=1.1) +
  facet_grid(~region_sample, scale="free") + theme_bw() +
  scale_x_discrete(limits=c("CA","NW","SW")) + 
  theme(text = element_text(size=19),  panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.border=element_blank(), axis.line= element_line(colour = "black")) +
  labs(x="", y="Fossil generation\n(5th - 95th)", colour="") + scale_y_continuous(labels = scales::percent) + guides(fill="none") +
  geom_hline(yintercept = 0, linetype="dashed") 
ggsave("Figure2D.pdf",width = 3.5, height = 3)

