## 3.3 Relationship between focal company and suppliers in grievance cases and supplier engagement 

# Freq table of sourcing relationships
table_analysis %>% 
  filter(GC_sourcing != "NA") %>% 
  group_by(GC_sourcing) %>% summarise(Count = n()) %>%
  mutate(Percent = round((Count/sum(Count)*100), digits = 1))  %>%
  write.csv(quote=F, row.names=F)

# Figure 5. Count of engagement channel in each sourcing relationship. Proportion of engagement channel in each sourcing relationship
# Bar graph, counts of engagement channel in each  sourcing relationship
plot_engagement_count <- 
  table_analysis %>%
  melt(measure.vars= c('Channel_direct', 'Channel_indirect', 'Channel_strategic_alliance', 'Channel_third_party'),
       variable.name = "channel") %>%
  filter(!GC_sourcing %in% c('Unable to determine', 'Not in supply chain', 'NA'), value== "1") %>%
  ggplot(aes(x= GC_sourcing, fill=channel)) + geom_bar() + 
  labs(x="Sourcing relationship", y="Count", fill="Engagement channel", title="(a)") + 
  scale_fill_brewer(palette="Dark2", 
                    labels = c("Direct", "Indirect", "Strategic alliance", "Third party")) +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size=11.5),
        legend.title = element_text(size=12), legend.text = element_text(size=11.5))
# Bar graph. proportions of engagement channel in each sourcing relationship
plot_engaggement_perc <-
  table_analysis %>% 
  melt(measure.vars= c('Channel_direct', 'Channel_indirect', 'Channel_strategic_alliance', 'Channel_third_party'),
       variable.name = "channel") %>%
  filter(!GC_sourcing %in% c('Unable to determine', 'Not in supply chain', 'NA'), value== "1") %>%
  ggplot(aes(x= GC_sourcing, fill=channel)) + geom_bar(position="fill") + 
  labs(x="Sourcing relationship", y="Proportion", fill="Engagement channel", title="(b)") +
  scale_fill_brewer(palette="Dark2", 
                    labels = c("Direct", "Indirect", "Strategic alliance", "Third party")) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size=11.5),
        legend.title = element_text(size=12), legend.text = element_text(size=11.5))
# Combined Figure 5
ggarrange(plot_engagement_count, plot_engaggement_perc, common.legend = T)

# Freq table of Figure 5
table_analysis %>% 
  melt(measure.vars= c('Channel_direct', 'Channel_indirect', 'Channel_strategic_alliance', 'Channel_third_party'),
       variable.name = "channel") %>%
  filter(!GC_sourcing %in% c('Unable to determine', 'Not in supply chain', 'NA'), value== "1") %>%
  group_by(GC_sourcing, channel) %>% summarise(Count = n()) %>%
  mutate(Percent = round((Count/sum(Count)*100), digits = 1))  %>% 
  write.csv(quote=F, row.names = F)