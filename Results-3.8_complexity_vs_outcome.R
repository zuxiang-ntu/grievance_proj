## 3.8 Grievance complexity vs. Grievance outcome

# Figure 10
table_analysis %>% 
  filter(Grievance_outcome != "NA", Complexity != "NA") %>%
  ggplot() + 
  geom_violin(aes(x= Grievance_outcome, y= Complexity), fill= '#1F78B4') +
  scale_x_discrete(labels= scales::label_wrap(10)) +
  scale_y_continuous(n.breaks = 8) +
  labs(x= "Grievance outcome") +
  theme(axis.title.x = element_text(vjust = -0.5, size= 12), axis.text.x = element_text(size= 11.5), 
        axis.title.y = element_text(size= 12), axis.text.y= element_text(size=11.5))

#####
# Code for additional info
# Mosaic plot with stats test
table_analysis %>% 
  filter(Grievance_outcome != "NA", Complexity != "NA") %>%
  select(Complexity, Grievance_outcome) %>% table() %>% vcd::mosaic(shade=T)
# Frequency table
table_analysis %>% 
  filter(Grievance_outcome != "NA") %>%
  select(Complexity, Grievance_outcome) %>% 
  sjtab(fun = "xtab", var.labels=c("Complexity", "Grievance_outcome"), 
        show.exp = F, show.row.prc = T, show.col.prc = F, show.legend = T, show.summary = T)
# Mosaic plot
table_analysis %>% 
  filter(Grievance_outcome != "NA", Complexity != "NA") %>%
  ggplot() + 
  geom_mosaic(aes(x= product(Grievance_outcome, Complexity), fill= Grievance_outcome), offset = 0.04) +
  scale_fill_brewer(palette= "Paired") +
  labs(x="Complexity score", y="Grievance outcome", fill="Grievance outcome") +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size=11.5))