## 3.11 Grievance theme vs. Grievance outcome

# Table 13
table_analysis %>% 
  filter(Status=="Closed") %>%
  mutate(Grievance_theme= factor(Grievance_theme, levels= c("Environmental", "Social", "Both"))) %>%
  select(Grievance_theme, Grievance_outcome) %>%
  sjtab(fun = "xtab", var.labels=c("Grievance theme", "Grievance outcome"), 
        show.exp = F, show.row.prc = T, show.col.prc = F, show.legend = T, show.summary = F)
# Figure 15
table_analysis %>% 
  filter(Status=="Closed", Grievance_outcome != "NA") %>%
  mutate(Grievance_theme= factor(Grievance_theme, levels= c("Environmental", "Social", "Both"))) %>%
  ggplot() + 
  geom_mosaic(aes(x= product(Grievance_outcome, Grievance_theme), fill= Grievance_outcome), offset=0.02) +
  scale_fill_brewer(palette= "Paired") +
  labs(x="Grievance theme", y="Grievance outcome", fill="Grievance outcome") +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size=11.5))
# Mosaic plot with stats test
table_analysis %>% 
  filter(Status=="Closed", Grievance_outcome != "NA") %>%
  mutate(Grievance_theme= factor(Grievance_theme, levels= c("Environmental", "Social", "Both"))) %>%
  select(Grievance_theme, Grievance_outcome) %>% table() %>% vcd::mosaic(shade=T)