## 3.9 Grievance complexity vs. Grievance outcome

# Table 11
table_analysis %>% 
  filter(Grievance_outcome != "NA") %>%
  select(Complexity, Grievance_outcome) %>% 
  sjtab(fun = "xtab", var.labels=c("Complexity", "Grievance_outcome"), 
        show.exp = F, show.row.prc = T, show.col.prc = F, show.legend = T, show.summary = F)
# Figure 13
table_analysis %>% 
  filter(Grievance_outcome != "NA", Complexity != "NA") %>%
  ggplot() + 
  geom_mosaic(aes(x= product(Grievance_outcome, Complexity), fill= Grievance_outcome), offset = 0.04) +
  scale_fill_brewer(palette= "Paired") +
  labs(x="Complexity score", y="Grievance outcome", fill="Grievance outcome") +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size=11.5))
# Mosaic plot with stats test
table_analysis %>% 
  filter(Grievance_outcome != "NA", Complexity != "NA") %>%
  select(Complexity, Grievance_outcome) %>% table() %>% vcd::mosaic(shade=T)
