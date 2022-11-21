## 3.3 Relationship between focal company and suppliers in grievance cases and supplier engagement 


# Figure 3a-b
# Figure 3a. No. of cases in each sourcing relationship
plot_sourcing_count <- 
  table_analysis %>% 
  ggplot(aes(y= fct_rev(fct_infreq(GC_sourcing)))) + geom_bar(fill="#1F78B4") + 
  labs(y="Sourcing relationship", x="No. of cases", title="(a)") + 
  scale_y_discrete(labels= scales::label_wrap(10)) +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size=11.5),
        legend.title = element_text(size=12), legend.text = element_text(size=11.5))
# Figure 3b. proportions of engagement channel in each sourcing relationship
plot_engaggement_perc <-
  table_analysis %>% 
  melt(measure.vars= c('Channel_direct', 'Channel_indirect', 'Channel_strategic_alliance', 'Channel_third_party'),
       variable.name = "channel") %>%
  filter(!GC_sourcing %in% c('Unable to determine', 'Not in supply chain', 'NA'), value== "1") %>%
  ggplot(aes(y= GC_sourcing, fill=channel)) + geom_bar(position="fill") + 
  labs(y="Sourcing relationship", x="Proportion", fill="Engagement channel", title="(b)") +
  scale_fill_brewer(palette="Dark2", 
                    labels = c("Direct", "Indirect", "Strategic alliance", "Third party")) +
  scale_x_continuous(labels = scales::percent) +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size=11.5),
        legend.title = element_text(size=12), legend.text = element_text(size=11.5))
# Combined Figure 3a-b
ggarrange(plot_sourcing_count, plot_engaggement_perc, common.legend = T)

#####
# Code for additional info
# Freq table of sourcing relationships
table_analysis %>% 
  group_by(GC_sourcing) %>% 
  summarise(Count = n()) %>%
  mutate(Percent = round((Count/sum(Count)*100), digits = 1))
# Freq table of engagement channel grouped by sourcing relationship
table_analysis %>% 
  melt(measure.vars= c('Channel_direct', 'Channel_indirect', 'Channel_strategic_alliance', 'Channel_third_party'),
       variable.name = "channel") %>%
  filter(!GC_sourcing %in% c('Unable to determine', 'Not in supply chain', 'NA'), value== "1") %>%
  group_by(GC_sourcing, channel) %>% summarise(Count = n()) %>%
  mutate(Percent = round((Count/sum(Count)*100), digits = 1))  %>% 
  write.csv(quote=F, row.names = F)

