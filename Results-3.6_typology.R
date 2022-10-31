## 3.6. Typology of grievance cases 

# Table 10. Overview of grievance characteristics
table_analysis %>% 
  filter(Company != "NA") %>% 
  melt(measure.vars=c("GC_clarity", "GC_status", "GC_number", "GC_sourcing"),
       variable.name = "Characteristic", value.name = "Measure") %>%
  group_by(Characteristic) %>% count(Measure) %>% 
  mutate(Percent = round((100*n/sum(n)), digits = 1)) %>% 
  write.csv(quote=F, row.names=F)
# Figures version of Table 10
plot_clarity <- 
  table_analysis %>% 
  filter(Company != "NA") %>%
  mutate(GC_clarity = factor(GC_clarity, levels = c("Clear", "Unclear", "NA"))) %>% 
  ggplot(aes(y=fct_rev(GC_clarity), fill="")) + 
  geom_bar() +
  scale_y_discrete(labels = c("NA (0)", "Unclear (2)", "Clear (1)")) +
  scale_fill_manual(values="#1F78B4", guide='none') +
  labs(x= "Count", y= "Clarity (Score)", title="(a)") +
  theme(axis.title.y = element_text(vjust = 2)) 
plot_status <- 
  table_analysis %>%
  filter(Company != "NA") %>%
  mutate(GC_status = factor(GC_status, levels=c("NA", "Low profile", "High profile", "Internal reporting"))) %>% 
  ggplot(aes(y = GC_status, fill="")) +
  geom_bar() +
  scale_fill_manual(values ="#33A02C", guide="none") +
  scale_y_discrete(labels=c("NA (0)", "Low profile (2)", "High profile (1)", "Internal reporting (1)")) +
  labs(y="Status of grievance raiser (Score)", x="Count", title="(b)") 
plot_number <- 
  table_analysis %>%
  filter(Company != "NA") %>%
  ggplot(aes(y = GC_number, fill= "")) +
  geom_bar()+
  scale_fill_manual(values="#E31A1C", guide="none") +
  labs(x="Count", y="Number of aggrieved parties", title="(c)")
plot_sourcing <- 
  table_analysis %>% 
  filter(Company != "NA", GC_sourcing != "Unable to determine") %>%
  mutate(GC_sourcing = factor(GC_sourcing, levels=c("Not in supply chain", "Indirect", "Direct", "Vertical"))) %>%
  ggplot(aes(y = GC_sourcing, fill="")) +
  geom_bar() +
  scale_fill_manual(values="#FF7F00", guide="none") +
  scale_y_discrete(labels=c("Not in supply chain (0)", "Indirect (3)", "Direct (2)", "Vertical (1)")) +
  labs(x="Count", y= 'Sourcing relationship (Score)', title='(d)') 
# Combined figure
ggarrange(plot_clarity, plot_status, plot_number, plot_sourcing,
          ncol=2, nrow=2)

# Stats table for complexity score
table_analysis %>% filter(Complexity != "NA") %>%
  summarise(min = min(Complexity, na.rm = T), quant1 = quantile(Complexity, probs = 0.25, na.rm = T), 
            med = median(Complexity, na.rm = T), mean = mean(Complexity, na.rm = T),
            quant3 = quantile(Complexity, probs = 0.75, na.rm = T), max = max(Complexity, na.rm = T)) %>%
  write.csv(quote=F, row.names = F)
table_analysis %>% count(Complexity > 0)

# Figure 9. Summary of grievance complexity scores 
table_analysis %>% 
  filter(Complexity != "NA") %>%
  ggplot() + 
  geom_histogram(aes(x = Complexity, alpha = 1), binwidth=1, boundary = 0, closed = "left") +
  scale_x_continuous(n.breaks = 7) +
  geom_vline(xintercept = 5, size = 1) + geom_vline(xintercept = 5.28, size = 1, linetype = 2) +
  annotate("text", x = c(4.5, 5.8), y = c(110, 110), 
           label = c("Median: 5", "Mean: 5.28"),
           fontface="bold") +
  labs(x = "Complexity score", y = "Count") +
  theme(legend.position = "none", axis.title = element_text(size = 12), axis.text = element_text(size=11.5))
