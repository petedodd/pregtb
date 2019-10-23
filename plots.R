
library(scales)
library(svglite)
library(getTBinR)

source(here::here("pregTB_births.R"))

# Plot of TB incidence during pregnancy and postpartum for the WHO regions
regions_plot <- summary_regions_byagegroup %>% 
  select(g_whoregion, age_group, pregTBI_best, ppTBI_best) %>%
  dplyr::rename(Pregnancy = pregTBI_best, Postpartum = ppTBI_best) %>%
  gather(Period, value, c("Pregnancy", "Postpartum")) %>% 
  # mutate(TBI_rate = value/births_best*1000) %>%  
  ggplot(aes(x=age_group,y=value,fill=Period)) +
  geom_bar(stat="identity",position="dodge") +
  # scale_fill_discrete(name="variable",
  #                     breaks=c(1, 2),
  #                     labels=c("Pregnancy", "Postpartum")) +
  xlab("Age in years")+ylab("Estimated number of TB incident cases (all forms)") + facet_wrap(~g_whoregion) +
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle=45, hjust=1))+
  scale_y_continuous(labels = comma)
regions_plot

# Plot of TB incidence during pregnancy and postpartum for the WHO regions
# includes error bars based on low and hi values 
regions <- summary_regions_byagegroup %>% 
  select(g_whoregion, age_group, pregTBI_best, ppTBI_best) %>%
  dplyr::rename(Pregnancy = pregTBI_best, Postpartum = ppTBI_best) %>%
  gather(Period, best, c("Pregnancy", "Postpartum")) 
# mutate(TBI_rate = value/births_best*1000) 

regions_lo <- summary_regions_byagegroup %>% 
  select(g_whoregion, age_group, pregTBI_lo, ppTBI_lo) %>%
  dplyr::rename(Pregnancy = pregTBI_lo, Postpartum = ppTBI_lo) %>%
  gather(Period, lo, c("Pregnancy", "Postpartum"))

regions_hi <- summary_regions_byagegroup %>% 
  select(g_whoregion, age_group, pregTBI_hi, ppTBI_hi) %>%
  dplyr::rename(Pregnancy = pregTBI_hi, Postpartum = ppTBI_hi) %>%
  gather(Period, hi, c("Pregnancy", "Postpartum"))

regions_hilo <- regions_lo %>% left_join(regions_hi, by=c("g_whoregion", "age_group", "Period"))

regions_hilo <- regions_hilo %>% left_join(regions, by=c("g_whoregion", "age_group", "Period"))

regions_plot2 <- regions_hilo%>%  
  ggplot(aes(x=age_group,y=best,fill=Period)) +
  geom_bar(stat="identity",position="dodge") +
  # scale_fill_discrete(name="variable",
  #                     breaks=c(1, 2),
  #                     labels=c("Pregnancy", "Postpartum")) +
  xlab("Age in years")+ylab("Estimated number of TB incident cases (all forms)") + facet_wrap(~g_whoregion) +
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle=45, hjust=1))+
  scale_y_continuous(labels = comma) + 
  geom_errorbar(aes(ymin=lo, ymax=hi), width=.2,
                position=position_dodge(.9))
regions_plot2

# save plots as svg # open in inkspace and save as emf for import into powerpoint
ggsave(plot=regions_plot,filename=here::here("plots/TB incidence.svg"),
       width=10, height=8, dpi=400)
ggsave(plot=regions_plot2,filename=here::here("plots/TB incidence_ebar.svg"),
       width=10, height=8, dpi=400)

