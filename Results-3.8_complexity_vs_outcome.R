## 3.8 Grievance complexity vs. Grievance outcome

# Table 11
table_analysis %>% 
  filter(Grievance_outcome != "NA") %>%
  select(Complexity, Grievance_outcome) %>% 
  sjtab(fun = "xtab", var.labels=c("Complexity", "Grievance_outcome"), 
        show.exp = F, show.row.prc = T, show.col.prc = F, show.legend = T, show.summary = T)
# Figure 10
table_analysis %>% 
  filter(Grievance_outcome != "NA", Complexity != "NA") %>%
  ggplot() + 
  geom_mosaic(aes(x= product(Grievance_outcome, Complexity), fill= Grievance_outcome), offset = 0.04) +
  scale_fill_brewer(palette= "Paired") +
  labs(x="Complexity score", y="Grievance outcome", fill="Grievance outcome") +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size=11.5))
#####
# Code for additional info
# Mosaic plot with stats test
table_analysis %>% 
  filter(Grievance_outcome != "NA", Complexity != "NA") %>%
  select(Complexity, Grievance_outcome) %>% table() %>% vcd::mosaic(shade=T)
# Multinomial logistic regression
multi_model <- multinom(formula = Grievance_outcome ~ Complexity+ GC_sourcing+ Grievance_theme,
                        data = table_analysis)
summary(multi_model)
# calculate z-statistics of coefficients
z_stats <- summary(multi_model)$coefficients/
  summary(multi_model)$standard.errors
# convert to p-values
p_values <- (1 - pnorm(abs(z_stats)))*2
# display p-values in transposed data frame
data.frame(t(p_values))
# display odds ratios in transposed data frame
odds_ratios <- exp(summary(multi_model)$coefficients)
data.frame(t(odds_ratios))
