## Results 3.2: Theme, sub-theme, issues

# Figure 2a-f. 
# Data objects to create plot legend
# table of sub themes 
sub_theme <- table_analysis %>% 
  # reshape data from wide to long format
  melt(measure.vars = string_subtheme, variable.name='Sub_theme') %>% 
  filter(value=="1", Grievance_theme == "Environmental") %>% 
  select(Sub_theme)
# colour palette for sub theme figures, Assign specific colours to sub theme table above
cols <- setNames(c("#33A02C", "#B15928", "#FF7F00", "#FFFF99", "#FB9A99", 
                   "#A6CEE3", "#B2DF8A", "#E31A1C", "#CAB2D6", "#6A3D9A"), levels(sub_theme)) 
# colour palette for issue figures. Assign specific colours to sub theme vector string
cols2 <- setNames(c("#33A02C", "#B15928", "#FF7F00", "#FFFF99", "#FB9A99", 
                    "#A6CEE3", "#B2DF8A", "#E31A1C", "#CAB2D6", "#6A3D9A"), 
                  c("Deforestation", "Peat development", "Fire", "Biodiversity", "Pollution",
                    "Labour", "Land", "Human rights abuse", "Community", "Corruption"))       

# Figure 2a. No. of cases under env sub-theme
env_sub_fig <- 
  table_analysis %>%
  melt(measure.vars = string_subtheme, variable.name='Sub_theme') %>% 
  filter(value=="1", Grievance_theme == "Environmental") %>%
  ggplot(aes(y = fct_rev(fct_infreq(Sub_theme)), fill=Sub_theme)) + 
  geom_bar() + 
  scale_y_discrete(labels=c("Pollution", "Biodiversity", "Fire", "Peat development", "Deforestation")) +
  # manually fill figure using the sub theme colour palette object [cols]
  scale_fill_manual(values=cols[1:5], 
                    labels=c("Deforestation", "Peat development", "Fire", "Biodiversity", "Pollution")) +
  labs(y = 'Sub theme', x="No. of cases", fill="Sub-theme", title="(a)") +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size=11.5),
        legend.title = element_text(size=12), legend.text = element_text(size=11.5))
# Figure 2b. No. of cases under env issues
env_iss_fig <- table_analysis %>% 
  melt(measure.vars=string_issue, variable.name="Issue") %>%
  filter(value=="1", Grievance_theme=="Environmental") %>%
  ggplot() + 
  geom_bar(aes(y=fct_rev(fct_infreq(Issue)), alpha=0.5)) +
  scale_y_discrete(labels = c("Pollution", "Deforestation preparation", "Wildlife threat", "Peatland burning",
                              "Forest burning", "Forest fire", "Habitat loss", "Source conflict oil",
                              "Peat development", "Deforestation")) +
  # manually colour each bar according to sub theme
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
  # assign fill colour to issue colour palette data object [cols2]
  scale_fill_manual(values=cols2, breaks = c("Deforestation", "Peat development", "Fire", "Biodiversity", "Pollution")) +
  labs(x = "No. of cases", y="Issue", fill="Sub-theme", title="(b)") +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size=11.5),
        legend.title = element_text(size=12), legend.text = element_text(size=11.5))
# Combined figure 2a-b. Full figure 2a-f will be combined again in powerpoint
ggarrange(env_sub_fig, env_iss_fig,
          ncol= 2, common.legend = T)

# Figure 2c. No. of cases under social sub-theme
soc_sub_fig <- table_analysis %>% 
  melt(measure.vars = string_subtheme, variable.name='Sub_theme') %>% 
  filter(value=="1", Grievance_theme == "Social") %>%
  ggplot(aes(y = fct_rev(fct_infreq(Sub_theme)), fill=Sub_theme)) + geom_bar() +
  scale_y_discrete(labels=c("Corruption", "Community", "Human rights abuse", "Labour", "Land")) +
  # manually fill figure using the sub theme colour palette object [cols]
  scale_fill_manual(values=cols[6:10], 
                    breaks=c("ST_land", "ST_labour", "ST_human_rights", "ST_community", "ST_corruption"), 
                    labels=c("Land", "Labour", "Human rights abuse", "Community", "Corruption")) +
  labs(x= "No. of cases", y = 'Sub-theme', fill="Sub-theme", title="(c)") +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size=11.5),
        legend.title = element_text(size=12), legend.text = element_text(size=11.5))
# Figure 2d. No. of cases under social issues
soc_iss_fig <- table_analysis %>% 
  melt(measure.vars=string_issue, variable.name="Issue") %>%
  filter(value=="1", Grievance_theme=="Social") %>%
  ggplot() + 
  geom_bar(aes(y=fct_rev(fct_infreq(Issue)), alpha=0.5)) +
  scale_y_discrete(labels=c("Obstruction of justice", "Bribery", "Community development", "Smallholder", "Eviction", 
                            "Violence", "Freedom of association", "Illegal land use", "FPIC", "Intimidation", 
                            "Land grabbing", "Forced labour", "Social security", "Child labour", "Land contestation", 
                            "Employment security", "Health and safety", "Wage and remuneration")) + 
  # manually colour each bar according to sub theme
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
  # assign fill colour to issue colour palette data object [cols2] 
  scale_fill_manual(values=cols2, breaks=c("Labour", "Land", "Human rights abuse", "Community", "Corruption")) +
  labs(x = "No. of cases", y="Issue", fill="Sub-theme", title="(d)") +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size=11.5),
        legend.title = element_text(size=12), legend.text = element_text(size=11.5))
# Combined figure 2c-d. Full figure 2a-f will be combined again in powerpoint
ggarrange(soc_sub_fig, soc_iss_fig, 
          ncol= 2, common.legend = T)

# Figure 2e. No. of cases under both sub-theme
both_sub_fig <- table_analysis %>% 
  melt(measure.vars = string_subtheme, variable.name='Sub_theme') %>% 
  filter(value=="1", Grievance_theme == "Both") %>%
  ggplot(aes(y = fct_rev(fct_infreq(Sub_theme)), fill=Sub_theme)) + geom_bar() + 
  # manually fill figure using sub theme colour palette object [cols]
  scale_fill_manual(values=cols, labels=c("Deforestation", "Peat development", "Fire", "Biodiversity", "Pollution",
                                          "Labour", "Land", "Human rights abuse", "Community", "Corruption")) + 
  scale_y_discrete(labels=c("Corruption", "Biodiversity", "Fire", "Labour", "Pollution", "Peat development",
                            "Human rights abuse", "Land", "Community", "Deforestation")) +
  labs(y = 'Sub-theme', x="No. of cases", fill="Sub-theme", title = "(e)") +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size=11.5),
        legend.title = element_text(size=12), legend.text = element_text(size=11.5))
# Figure 2f. No. of cases under both issues
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
  # manually colour each bar according to sub theme
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
  # assign fill colour to issue colour palette data object [cols2]
  scale_fill_manual(values=cols2) +
  labs(x = "No. of cases", y="Issue", title="(f)", fill="Sub-theme") +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size=11.5),
        legend.title = element_text(size=12), legend.text = element_text(size=11.5))
# Combined Figure 2e-f. Full figure 2a-f will be combined again in powerpoint
ggarrange(both_sub_fig, both_iss_fig, 
          ncol= 2, common.legend = T)
#####
# Code for additional info
# Freq table of grievance theme
table_analysis %>% 
  filter(Grievance_theme != "NA") %>%
  group_by(Grievance_theme) %>% summarise(Count = n()) %>%
  mutate(Percent = round((Count/sum(Count)*100), digits = 1)) %>% 
  arrange(-Count)
# Frequency table of Sub-theme
table_analysis %>% # All sub-themes
  melt(measure.vars = string_subtheme, variable.name='Sub_theme') %>% filter(value=="1") %>% 
  group_by(Sub_theme) %>% summarise(Count = n()) %>%
  mutate(Percent = round((Count/sum(Count)*100), digits = 1)) %>% 
  arrange(-Count) %>% write.csv(quote=F, row.names = F)
# Frequency table of Issue
table_analysis %>% # All issues
  melt(measure.vars = string_issue, variable.name="Issue") %>% 
  filter(value == "1") %>%
  group_by(Issue) %>% summarise(Count = n()) %>%
  mutate(Percent = round((Count/sum(Count)*100), digits = 1))  %>%
  arrange(-Count) %>% write.csv(quote=F, row.names=F)
# Frequency table of sub theme grouped by theme
table_analysis %>% # All sub-themes
  melt(measure.vars = string_subtheme, variable.name='Sub_theme') %>% filter(value=="1") %>% 
  group_by(Grievance_theme, Sub_theme) %>% summarise(Count = n()) %>%
  mutate(Percent = round((Count/sum(Count)*100), digits = 1)) 
# Frequency table of issues grouped by theme
table_analysis %>% # All sub-themes
  melt(measure.vars = string_issue, variable.name='issue') %>% filter(value=="1") %>% 
  group_by(Grievance_theme, issue) %>% summarise(Count = n()) %>%
  mutate(Percent = round((Count/sum(Count)*100), digits = 1)) 
