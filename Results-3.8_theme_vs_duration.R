## 3.8 Theme vs Duration

# Stats table
stats_theme_duration <-
  table_analysis %>% 
  filter(Status=="Closed", Grievance_theme != "NA") %>%
  mutate(Grievance_theme = factor(Grievance_theme, levels= c("Environmental", "Social", "Both"))) %>%
  group_by(Grievance_theme) %>%   
  summarise(Median = round((median(Duration_lodged_closed, na.rm = T)), digits=1),
            Mean = round((mean(Duration_lodged_closed, na.rm = T)), digits=1)) %>%
  melt(id.vars="Grievance_theme") 
# Figure 12. Grievance theme vs DUration
table_analysis %>% 
  filter(Status=="Closed", Grievance_theme != "NA") %>%
  mutate(Grievance_theme = factor(Grievance_theme, levels= c("Environmental", "Social", "Both"))) %>%
  ggplot(aes(x = Grievance_theme, y = Duration_lodged_closed)) + 
  geom_jitter(position = position_jitter(0.2, 0.2), alpha=0.2) +
  geom_line(data= stats_theme_duration, 
            aes(x= Grievance_theme, y= value, group= variable, linetype= variable),
            size= 1) +
  labs(x="Grievance theme", y="Duration (months)", linetype="") +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size=11.5))