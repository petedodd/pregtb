
library(scales)
library(svglite)
library(getTBinR)

source(here::here("TBburden/R", "pregTB_outputs.R"))

# Plot of TB incidence all sexes for the WHO regions
unique(df$age_group)
regions_sex <- df %>% 
  # dplyr::rename(Pregnancy = pregTBI_best, Postpartum = ppTBI_best) %>%
  # gather(Period, value, c("Pregnancy", "Postpartum")) %>% 
  # mutate(TBI_rate = value/births_best*1000) %>%  
  ggplot(aes(x=age_group,y=TBI_best,fill=age_group)) +
  geom_bar(stat="identity",position="dodge") +
  # scale_fill_discrete(name="g_whoregion") +
  xlab("Age in years")+ylab("Estimated number of TB incident cases (all forms)") + 
  facet_wrap(~g_whoregion, scales = 'free') +
  theme(text = element_text(size=14),
        axis.text.x = element_text(angle=45, hjust=1))+
  scale_y_continuous(labels = comma)
regions_sex

default_colors <- scales::hue_pal()(2)
# Plot of TB incidence during pregnancy and postpartum for the WHO regions
names(summary_regions_byagegroup_adjusted)
regions_plot <- summary_regions_byagegroup_adjusted %>% 
  select(g_whoregion, age_group, contains('P_best')) %>%
  dplyr::rename(Pregnancy = TBI.P_best, Postpartum = TBI.PP_best) %>%
  # gather(Period, value, c("Pregnancy", "Postpartum")) %>% 
  pivot_longer(cols = -c(g_whoregion, age_group), names_to = "Period", values_to = "value") %>%
  mutate(value = as.numeric(gsub(',', '', value))) %>%
  # mutate(TBI_rate = value/births_best*1000) %>%  
  ggplot(aes(x=age_group,y=value,fill=Period)) +
  geom_bar(stat="identity",position="dodge") +
  # scale_fill_discrete(name="variable",
  #                     breaks=c(1, 2),
  #                     labels=c("Pregnancy", "Postpartum")) +
  xlab("Age in years")+ylab("Estimated number of TB incident cases (all forms)") + 
  facet_wrap(.~g_whoregion, scales = 'free') +
  theme(text = element_text(size=14),
        axis.text.x = element_text(angle=45, hjust=1))+
  scale_y_continuous(labels = comma, breaks = pretty_breaks()) +
  scale_fill_manual(breaks = c("Pregnancy", "Postpartum"), values = rev(default_colors))
regions_plot

# Plot of TB incidence during pregnancy and postpartum for the WHO regions
# includes error bars based on low and hi values 
regions <- summary_regions_byagegroup_adjusted %>% 
  select(g_whoregion, age_group, contains('P_best')) %>%
  dplyr::rename(Pregnancy = TBI.P_best, Postpartum = TBI.PP_best) %>%
  gather(Period, best, c("Pregnancy", "Postpartum")) 
# mutate(TBI_rate = value/births_best*1000) 

regions_lo <- summary_regions_byagegroup_adjusted %>% 
  select(g_whoregion, age_group, contains('P_lo')) %>%
  dplyr::rename(Pregnancy = TBI.P_lo, Postpartum = TBI.PP_lo) %>%
  gather(Period, lo, c("Pregnancy", "Postpartum"))

regions_hi <- summary_regions_byagegroup_adjusted %>% 
  select(g_whoregion, age_group, contains('P_hi')) %>%
  dplyr::rename(Pregnancy = TBI.P_hi, Postpartum = TBI.PP_hi) %>%
  gather(Period, hi, c("Pregnancy", "Postpartum"))

regions_hilo <- regions_lo %>% left_join(regions_hi, by=c("g_whoregion", "age_group", "Period"))

regions_hilo <- regions_hilo %>% 
  left_join(regions, by=c("g_whoregion", "age_group", "Period")) |> 
  mutate(Period = factor(Period, levels = c("Pregnancy", "Postpartum")))

regions_plot2 <- regions_hilo%>%  
  mutate(across(c(best, lo, hi), ~as.numeric(gsub(',', '', .)))) %>%
  ggplot(aes(x=age_group,y=best,fill=Period)) +
  geom_bar(stat="identity",position="dodge") +
  # scale_fill_discrete(name="variable",
  #                     breaks=c(1, 2),
  #                     labels=c("Pregnancy", "Postpartum")) +
  xlab("Age in years")+ylab("Estimated number of TB incident cases (all forms)") + 
  facet_wrap(.~g_whoregion, scales = 'free') +
  theme(text = element_text(size=14),
        axis.text.x = element_text(angle=45, hjust=1),
        legend.title = element_blank(),
        legend.position = 'top')+ 
  geom_errorbar(aes(ymin=lo, ymax=hi), width=.2,
                position=position_dodge(.9)) +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(breaks = c("Pregnancy", "Postpartum"), values = rev(default_colors))
regions_plot2

# save plots as svg # open in inkspace and save as emf for import into powerpoint
ggsave(plot=regions_plot,filename=here::here("TBburden/plots/TB incidence.svg"),
       width=10, height=8, dpi=600)
ggsave(plot=regions_plot,filename=here::here("TBburden/plots/TB incidence.png"),
       width=10, height=8, dpi=600)
ggsave(plot=regions_plot2,filename=here::here("TBburden/plots/TB incidence_ebar.svg"),
       width=10, height=8, dpi=600)
ggsave(plot=regions_plot2,filename=here::here("TBburden/plots/TB incidence_ebar.png"),
       width=10, height=8, dpi=600)

# Plot of TB incidence during pregnancy and postpartum for the WHO regions
# includes error bars based on low and hi values 

names(summary_regions_byagegroup_adjusted)
regions <- summary_regions_byagegroup_adjusted %>% 
  select(g_whoregion, age_group, contains('0_best'), contains('1_best')) %>%
  pivot_longer(cols = contains('best'), names_to = "Period", values_to = "best") %>%
  mutate(Period = gsub('_best', '', Period)) 
# mutate(TBI_rate = value/births_best*1000) 

regions_lo <- summary_regions_byagegroup_adjusted %>% 
  select(g_whoregion, age_group, contains('0_lo'), , contains('1_lo')) %>%
  pivot_longer(cols = contains('lo'), names_to = "Period", values_to = "lo") %>% 
  mutate(Period = gsub('_lo', '', Period))

regions_hi <- summary_regions_byagegroup_adjusted %>% 
  select(g_whoregion, age_group, contains('0_hi'), , contains('1_hi')) %>%
  pivot_longer(cols = contains('hi'), names_to = "Period", values_to = "hi") %>%
  mutate(Period = gsub('_hi', '', Period))

regions_hilo <- regions_lo %>% left_join(regions_hi, by=c("g_whoregion", "age_group", "Period"))

unique(regions_hilo$Period)

regions_hilo <- regions_hilo %>% 
  left_join(regions, by=c("g_whoregion", "age_group", "Period")) |> 
  mutate(Period = factor(Period, 
                         levels = c(
                           "TBI.PH0",
                           "TBI.PH1",
                           "TBI.PPH0",
                           "TBI.PPH1"),
                         labels = c(
                           "Pregnancy living without HIV",
                           "Pregnancy living with HIV",
                           "Postpartum living without HIV",
                           "Postpartum living with HIV"
                         )))

regions_plot3 <- regions_hilo%>%  
  mutate(across(c(best, lo, hi), ~as.numeric(gsub(',', '', .)))) %>%
  ggplot(aes(x=age_group,y=best,fill=Period)) +
  geom_bar(stat="identity",position="dodge") +
  # scale_fill_discrete(name="variable",
  #                     breaks=c(1, 2),
  #                     labels=c("Pregnancy", "Postpartum")) +
  xlab("Age in years")+ylab("Estimated number of TB incident cases (all forms)") + 
  facet_wrap(.~g_whoregion, scales = 'free') +
  theme(text = element_text(size=14),
        axis.text.x = element_text(angle=45, hjust=1),
        legend.title = element_blank(),
        legend.position = 'top')+ 
  geom_errorbar(aes(ymin=lo, ymax=hi), width=.5,
                position=position_dodge(.9)) +
  scale_y_continuous(labels = comma) 
# ggthemes::scale_fill_colorblind() 
regions_plot3

ggsave(plot=regions_plot3,filename=here::here("TBburden/plots/TBHIVincidence.svg"),
       width=10, height=8, dpi=600)
ggsave(plot=regions_plot3,filename=here::here("TBburden/plots/TBHIVincidence.png"),
       width=10, height=8, dpi=600)

