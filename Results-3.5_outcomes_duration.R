## 3.5 Grievance outcomes and duration of grievance resolution 

# Figure 7. No. cases related to grievance outcomes
table_analysis %>% 
  filter(Grievance_outcome != "NA") %>%
  ggplot(aes(y= fct_rev(fct_infreq(Grievance_outcome)))) + 
  geom_bar(fill= "#1F78B4") + 
  labs(x='No. of cases', y='Grievance outcomes') +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size=11.5),
        legend.title = element_text(size=12), legend.text = element_text(size=11.5))
# Figure 8. Frequency of cases based on duration of grievance resolution, with mean and median
table_analysis %>% 
  filter(Duration_lodged_closed != "NA") %>%
  ggplot(aes(x = Duration_lodged_closed)) + 
  geom_histogram(bins = 30, boundary = 0, closed = "left", fill= '#1F78B4') + 
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8 ,10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30,
                                32, 34, 36, 38, 40, 42, 44, 46, 48, 50, 52, 54, 56, 58, 
                                60, 62, 64, 66, 68), minor_breaks = NULL) +
  geom_vline(xintercept = 11.4, size = 1, linetype= 2) +               # median line
  geom_vline(xintercept = 17.1, size = 1, linetype = 1) + # mean line
  annotate("text", x = c(6.2, 22.5), y = c(22, 22), 
           label = c("Median: 11.4", "Mean: 17.1"),
           fontface="bold", angle=45) +
  labs(x = "Months", y = "No. of cases") +
  theme(legend.position = "none", 
        axis.title = element_text(size = 12), axis.text = element_text(size=11.5))
#####
# Code for additional info
# Freq table of outcomes
table_analysis %>% 
  filter(!is.na(Grievance_outcome)) %>%
  group_by(Grievance_outcome) %>% summarise(Count = n()) %>%
  mutate(Percent = round((Count/sum(Count)*100), digits = 1))
# Summary stats of duration of resolution
table_analysis %>%
  filter(Duration_lodged_closed != "NA") %>%
  summarise(count = n(), min = min(Duration_lodged_closed, na.rm = T), 
            quant1 = quantile(Duration_lodged_closed, probs = 0.25, na.rm = T),
            med = median(Duration_lodged_closed, na.rm = T), mean = mean(Duration_lodged_closed, na.rm = T), 
            quant3 = quantile(Duration_lodged_closed, probs = 0.75, na.rm = T), max = max(Duration_lodged_closed, na.rm = T))
