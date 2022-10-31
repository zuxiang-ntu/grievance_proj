## 3.10 Sourcing relationship vs. Grievance outcome

# Table 12
table_analysis %>%
  filter(Status=="Closed",
         !GC_sourcing %in% c("NA", "Unable to determine")) %>%
  mutate(GC_sourcing = factor(GC_sourcing, levels= c("Not in supply chain", "Vertical", "Direct", "Indirect"))) %>%
  select(GC_sourcing, Grievance_outcome) %>%
  sjtab(fun = "xtab", var.labels=c("Sourcing relationship", "Grievance outcome"), 
        show.exp = F, show.row.prc = T, show.col.prc = F, show.legend = T, show.summary = F)
# Figure 14
table_analysis %>% 
  filter(Status=="Closed", 
         !GC_sourcing %in% c("NA", "Unable to determine"),
         Grievance_outcome != "NA") %>%
  mutate(GC_sourcing = factor(GC_sourcing, levels= c("Not in supply chain", "Vertical", "Direct", "Indirect"))) %>%
  ggplot() + 
  geom_mosaic(aes(x= product(Grievance_outcome), fill= Grievance_outcome, conds= product(GC_sourcing)), offset= 0.04) +
  scale_fill_brewer(palette= "Paired") +
  labs(x="Sourcing relationship", y="Grievance outcome", fill="Grievance outcome") +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size=11.5),
        axis.text.x= element_text(hjust= 1, angle= 30))
# Mosaic plot with stats test
table_analysis %>% filter(Status=="Closed", GC_sourcing != "NA", Grievance_outcome != "NA") %>%
  mutate(GC_sourcing = factor(GC_sourcing, levels= c("Not in supply chain", "Vertical", "Direct", "Indirect"))) %>%
  select(GC_sourcing, Grievance_outcome) %>% table() %>%
  vcd::mosaic(shade=T)

