## Results 3.2: Theme, sub-theme, issues

# Freq tables
# Theme
table_analysis %>% 
  filter(Grievance_theme != "NA") %>%
  group_by(Grievance_theme) %>% summarise(Count = n()) %>%
  mutate(Percent = round((Count/sum(Count)*100), digits = 1)) %>%
  write.csv(quote = F)
# Sub-theme
table_analysis %>% # All sub-themes
  melt(measure.vars = string_subtheme, variable.name='Sub_theme') %>% filter(value=="1") %>% 
  group_by(Sub_theme) %>% summarise(Count = n()) %>%
  mutate(Percent = round((Count/sum(Count)*100), digits = 1)) %>% 
  write.csv(quote=F, row.names = F)
table_analysis %>% # Grouped by themes
  melt(measure.vars = string_subtheme, variable.name='Sub_theme') %>% filter(value=="1") %>%  
  group_by(Grievance_theme, Sub_theme) %>% summarise(Count = n()) %>%
  mutate(Percent = round((Count/sum(Count)*100), digits = 1)) %>% 
  write.csv(quote=F, row.names= F)
# Issue
table_analysis %>% # All issues
  melt(measure.vars = string_issue, variable.name="Issue") %>% 
  filter(value == "1") %>%
  group_by(Issue) %>% summarise(Count = n()) %>%
  mutate(Percent = round((Count/sum(Count)*100), digits = 1))  %>%
  write.csv(quote=F, row.names=F)
table_analysis %>% # Grouped by theme
  melt(measure.vars = string_issue, variable.name="Issue") %>% 
  filter(value == "1") %>%
  group_by(Grievance_theme, Issue) %>% summarise(Count = n()) %>%
  mutate(Percent = round((Count/sum(Count)*100), digits = 1))  %>% 
  write.csv(quote=F, row.names=F)

# Bar graphs
# Theme
table_analysis %>% filter(Grievance_theme != "NA") %>%
  ggplot(aes(x=Grievance_theme, fill=Grievance_theme)) + geom_bar() +
  labs(x="Grievance theme", y="Count", fill="Grievance theme", title="Count of Grievance themes")
# Sub-theme: all
table_analysis %>% # All sub-themes
  melt(measure.vars = string_subtheme, variable.name='Sub_theme') %>% 
  filter(value=="1") %>% 
  ggplot(aes(y = fct_rev(Sub_theme))) + geom_bar() + 
  labs(y = 'Sub theme', title="Count of sub themes")
# Issues: all
table_analysis %>% # All issues
  melt(measure.vars = string_issue, variable.name="Issue") %>% 
  filter(value == "1") %>% 
  ggplot(aes(y=fct_rev(Issue))) + geom_bar() +
  labs(x = "Count", y="Issue", title="Count of Issues")

# Figure 2. Count of sub-themes in env themed cases, count of issues in env themed cases
# Data objects to create plot legend 
sub_theme <- table_analysis %>% 
  melt(measure.vars = string_subtheme, variable.name='Sub_theme') %>% 
  filter(value=="1", Grievance_theme == "Environmental") %>% 
  select(Sub_theme)
cols <- setNames(brewer.pal(10, "Paired"), levels(sub_theme)) # color palette for env sub-theme figure
cols2 <- setNames(brewer.pal(10,"Paired"), c("Deforestation", "Peat development", "Fire", "Biodiversity", "Pollution",
                                             "Labour", "Land", "Human rights abuse", "Community", "Corruption"))
# Sub-theme figure 
env_sub_fig <- table_analysis %>%
  melt(measure.vars = string_subtheme, variable.name='Sub_theme') %>% 
  filter(value=="1", Grievance_theme == "Environmental") %>%
  ggplot(aes(y = fct_rev(fct_infreq(Sub_theme)), fill=Sub_theme)) + geom_bar() + 
  scale_y_discrete(labels=c("Pollution", "Biodiversity", "Fire", "Peat development", "Deforestation")) +
  scale_fill_manual(values=cols[1:5], 
                    labels=c("Deforestation", "Peat development", "Fire", "Biodiversity", "Pollution")) +
  labs(y = 'Sub theme', x="Count", fill="Sub-theme", title="(a)") +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size=11.5),
        legend.title = element_text(size=12), legend.text = element_text(size=11.5))
# Issues figure
env_iss_fig <- table_analysis %>% 
  melt(measure.vars=string_issue, variable.name="Issue") %>%
  filter(value=="1", Grievance_theme=="Environmental") %>%
  ggplot() + 
  geom_bar(aes(y=fct_rev(fct_infreq(Issue)), alpha=0.5)) +
  scale_y_discrete(labels = c("Pollution", "Deforestation preparation", "Wildlife threat", "Peatland burning",
                              "Forest burning", "Forest fire", "Habitat loss", "Source conflict oil",
                              "Peat development", "Deforestation")) +
  geom_rect(aes(xmin=0, xmax=1, ymin=1.55, ymax=2.45, fill="Deforestation")) +
  geom_rect(aes(xmin=0, xmax=40, ymin=7.55, ymax=8.45, fill="Deforestation")) +
  geom_rect(aes(xmin=0, xmax=255, ymin=9.55, ymax=10.45, fill="Deforestation")) +
  geom_rect(aes(xmin=0, xmax=100, ymin=8.55, ymax=9.45, fill="Peat development")) +
  geom_rect(aes(xmin=0, xmax=3, ymin=3.55, ymax=4.45, fill="Fire")) +
  geom_rect(aes(xmin=0, xmax=15, ymin=4.55, ymax=5.45, fill="Fire")) +
  geom_rect(aes(xmin=0, xmax=23, ymin=5.55, ymax=6.45, fill="Fire")) +
  geom_rect(aes(xmin=0, xmax=3, ymin=2.55, ymax=3.45, fill="Biodiversity")) +
  geom_rect(aes(xmin=0, xmax=33, ymin=6.55, ymax=7.45, fill="Biodiversity")) +
  geom_rect(aes(xmin=0, xmax=1, ymin=0.55, ymax=1.45, fill="Pollution")) +
  scale_alpha(guide = 'none') +
  scale_fill_manual(values=cols2, breaks = c("Deforestation", "Peat development", "Fire", "Biodiversity", "Pollution")) +
  labs(x = "Count", y="Issue", fill="Sub-theme", title="(b)") +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size=11.5),
        legend.title = element_text(size=12), legend.text = element_text(size=11.5))
# Combined figure 2
ggarrange(env_sub_fig, env_iss_fig, common.legend = T, ncol=2, align='h')

# Figure 3. Count of sub-themes from social themed cases, count of issues from social themed cases
# Sub-theme figure
soc_sub_fig <- table_analysis %>% 
  melt(measure.vars = string_subtheme, variable.name='Sub_theme') %>% 
  filter(value=="1", Grievance_theme == "Social") %>%
  ggplot(aes(y = fct_rev(fct_infreq(Sub_theme)), fill=Sub_theme)) + geom_bar() +
  scale_y_discrete(labels=c("Corruption", "Community", "Human rights abuse", "Labour", "Land")) +
  scale_fill_manual(values=cols[6:10], 
                    breaks=c("ST_land", "ST_labour", "ST_human_rights", "ST_community", "ST_corruption"), 
                    labels=c("Land", "Labour", "Human rights abuse", "Community", "Corruption")) +
  labs(x= "Count", y = 'Sub-theme', fill="Sub-theme", title="(a)") +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size=11.5),
        legend.title = element_text(size=12), legend.text = element_text(size=11.5))
# Issues figure
soc_iss_fig <- table_analysis %>% 
  melt(measure.vars=string_issue, variable.name="Issue") %>%
  filter(value=="1", Grievance_theme=="Social") %>%
  ggplot() + 
  geom_bar(aes(y=fct_rev(fct_infreq(Issue)), alpha=0.5)) +
  scale_y_discrete(labels=c("Obstruction of justice", "Bribery", "Community development", "Smallholder", "Eviction", 
                            "Violence", "Freedom of association", "Illegal land use", "FPIC", "Intimidation", 
                            "Land grabbing", "Forced labour", "Social security", "Child labour", "Land contestation", 
                            "Employment security", "Health and safety", "Wage and remuneration")) + 
  geom_rect(aes(xmin=0, xmax=1, ymin=0.55, ymax=1.45, fill="Corruption")) +
  geom_rect(aes(xmin=0, xmax=1, ymin=1.55, ymax=2.45, fill="Corruption")) +
  geom_rect(aes(xmin=0, xmax=2, ymin=2.55, ymax=3.45, fill="Community")) +
  geom_rect(aes(xmin=0, xmax=4, ymin=3.55, ymax=4.45, fill="Community")) +
  geom_rect(aes(xmin=0, xmax=5, ymin=4.55, ymax=5.45, fill="Human rights abuse")) +
  geom_rect(aes(xmin=0, xmax=6, ymin=5.55, ymax=6.45, fill="Human rights abuse")) +
  geom_rect(aes(xmin=0, xmax=6, ymin=6.55, ymax=7.45, fill="Labour")) +
  geom_rect(aes(xmin=0, xmax=8, ymin=7.55, ymax=8.45, fill="Land")) +
  geom_rect(aes(xmin=0, xmax=10, ymin=8.55, ymax=9.45, fill="Community")) +
  geom_rect(aes(xmin=0, xmax=12, ymin=9.55, ymax=10.45, fill="Human rights abuse")) +
  geom_rect(aes(xmin=0, xmax=12, ymin=10.55, ymax=11.45, fill="Land")) +
  geom_rect(aes(xmin=0, xmax=12, ymin=11.55, ymax=12.45, fill="Labour")) +
  geom_rect(aes(xmin=0, xmax=14, ymin=12.55, ymax=13.45, fill="Labour")) +
  geom_rect(aes(xmin=0, xmax=17, ymin=13.55, ymax=14.45, fill="Labour")) +
  geom_rect(aes(xmin=0, xmax=21, ymin=14.55, ymax=15.45, fill="Land")) + 
  geom_rect(aes(xmin=0, xmax=22, ymin=15.55, ymax=16.45, fill="Labour")) + 
  geom_rect(aes(xmin=0, xmax=23, ymin=16.55, ymax=17.45, fill="Labour")) +
  geom_rect(aes(xmin=0, xmax=35, ymin=17.55, ymax=18.45, fill="Labour")) +
  scale_alpha(guide = 'none') +
  scale_fill_manual(values=cols2, breaks=c("Labour", "Land", "Human rights abuse", "Community", "Corruption")) +
  labs(x = "Count", y="Issue", fill="Sub-theme", title="(b)") +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size=11.5),
        legend.title = element_text(size=12), legend.text = element_text(size=11.5))
# Combined figure 3
ggarrange(soc_sub_fig, soc_iss_fig, ncol=2, align="h", common.legend = T)

# Figure 4. Count of sub-themes from cases that are both env and soc, count of issues from cases that are both enc and soc
# Sub-theme figure
both_sub_fig <- table_analysis %>% 
  melt(measure.vars = string_subtheme, variable.name='Sub_theme') %>% 
  filter(value=="1", Grievance_theme == "Both") %>%
  ggplot(aes(y = fct_rev(fct_infreq(Sub_theme)), fill=Sub_theme)) + geom_bar() + 
  scale_fill_manual(values=cols, labels=c("Deforestation", "Peat development", "Fire", "Biodiversity", "Pollution",
                                          "Labour", "Land", "Human rights abuse", "Community", "Corruption")) +
  scale_y_discrete(labels=c("Corruption", "Biodiversity", "Fire", "Labour", "Pollution", "Peat development",
                            "Human rights abuse", "Land", "Community", "Deforestation")) +
  labs(y = 'Sub-theme', x="Count", fill="Sub-theme", title = "(a)") +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size=11.5),
        legend.title = element_text(size=12), legend.text = element_text(size=11.5))
# Issues figure
both_iss_fig <- table_analysis %>% 
  melt(measure.vars=string_issue, variable.name="Issue") %>%
  filter(value=="1", Grievance_theme=="Both") %>%
  ggplot() + 
  geom_bar(aes(y=fct_rev(fct_infreq(Issue)), alpha=0.5)) +
  scale_y_discrete(labels= c("Tax evasion", "Eviction", "Bribery", "Social security", "Health and safety",
                             "Wages and remuneration", "Illegal land use", "Employment security", "Habitat loss", 
                             "Forest fire", "Child labour", "Forest burning", "Violence", "Smallholder", 
                             "Land grabbing", "Pollution", "Intimidation", "Peat development", "Land contestation", 
                             "FPIC", "Deforestation")) +
  geom_rect(aes(xmin=0, xmax=1, ymin=0.55, ymax=1.45, fill="Corruption")) +
  geom_rect(aes(xmin=0, xmax=1, ymin=1.55, ymax=2.45, fill="Corruption")) +
  geom_rect(aes(xmin=0, xmax=2, ymin=2.55, ymax=3.45, fill="Corruption")) +
  geom_rect(aes(xmin=0, xmax=2, ymin=3.55, ymax=4.45, fill="Labour")) +
  geom_rect(aes(xmin=0, xmax=2, ymin=4.55, ymax=5.45, fill="Labour")) +
  geom_rect(aes(xmin=0, xmax=2, ymin=5.55, ymax=6.45, fill="Labour")) +
  geom_rect(aes(xmin=0, xmax=3, ymin=6.55, ymax=7.45, fill="Land")) +
  geom_rect(aes(xmin=0, xmax=3, ymin=7.55, ymax=8.45, fill="Labour")) +
  geom_rect(aes(xmin=0, xmax=3, ymin=8.55, ymax=9.45, fill="Biodiversity")) +
  geom_rect(aes(xmin=0, xmax=4, ymin=9.55, ymax=10.45, fill="Fire")) +
  geom_rect(aes(xmin=0, xmax=5, ymin=10.55, ymax=11.45, fill="Labour")) +
  geom_rect(aes(xmin=0, xmax=6, ymin=11.55, ymax=12.45, fill="Fire")) +
  geom_rect(aes(xmin=0, xmax=7, ymin=12.55, ymax=13.45, fill="Human rights abuse")) +
  geom_rect(aes(xmin=0, xmax=8, ymin=13.55, ymax=14.45, fill="Community")) +
  geom_rect(aes(xmin=0, xmax=11, ymin=14.55, ymax=15.45, fill="Land")) + 
  geom_rect(aes(xmin=0, xmax=11, ymin=15.55, ymax=16.45, fill="Pollution")) + 
  geom_rect(aes(xmin=0, xmax=16, ymin=16.55, ymax=17.45, fill="Human rights abuse")) +
  geom_rect(aes(xmin=0, xmax=16, ymin=17.55, ymax=18.45, fill="Peat development")) +
  geom_rect(aes(xmin=0, xmax=23, ymin=18.55, ymax=19.45, fill="Land")) +
  geom_rect(aes(xmin=0, xmax=31, ymin=19.55, ymax=20.45, fill="Community")) +
  geom_rect(aes(xmin=0, xmax=50, ymin=20.55, ymax=21.45, fill="Deforestation")) +
  scale_alpha(guide = 'none') +
  scale_fill_manual(values=cols2) +
  labs(x = "Count", y="Issue", title="(b)", fill="Sub-theme") +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size=11.5),
        legend.title = element_text(size=12), legend.text = element_text(size=11.5))
# Combined Figure 4
ggarrange(both_sub_fig, both_iss_fig, ncol=2, align="h", common.legend = T)