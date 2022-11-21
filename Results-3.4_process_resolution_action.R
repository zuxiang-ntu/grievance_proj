## 3.4 Grievance processes, resolution actions, and supplier engagement  

# Figure 4a-d.
# Data objects for plot legend
cols_foc_process <- setNames(brewer.pal(7, "Paired"), string_Focal_process)
cols_sup_process <- setNames(brewer.pal(7, "Paired"), string_Supplier_process)
cols_foc_resolution <- setNames(c("#A6CEE3", "#B2DF8A", "#FECC5C", "#FD8D3C", "#E31A1C"), 
                                c("FR_Remedial", "FR_Improve_prac", "FR_Coop_SCM", "FR_Coer_SCM", "FR_Re_entry"))
cols_sup_resolution <- setNames(c("#A6CEE3", "#B2DF8A", "#FB9A99", "#FFFFB2"), 
                                c("SR_Remedial", "SR_Improve_prac", "SR_Policy", "SR_SSCM"))
# Figure 4a. focal company process
plot_focal_process <- 
  table_analysis %>% 
  melt(measure.vars=string_Focal_process, variable.name="Focal_process") %>% 
  filter(value=="1") %>% 
  ggplot(aes(y = fct_rev(fct_infreq(Focal_process)))) + 
  geom_bar(fill= '#1F78B4') + 
  scale_y_discrete(labels= c("Mediation", "Negotiation", "Info facilitation", "Support", 
                             "Adjudication", "Socialisation", "Investigation")) +
  labs(x="No. of cases", y="", fill="Process", title="(a) Focal company process") +
  theme(title= element_text(size=10))
# Fgiure 4b. direct supplier process
plot_sup_process <-
  table_analysis %>%
  melt(measure.vars=string_Supplier_process, variable.name="Supplier_process") %>% 
  filter(value=="1") %>%
  ggplot(aes(y= fct_rev(fct_infreq(Supplier_process)))) + 
  geom_bar(fill= '#1F78B4') +
  scale_y_discrete(labels= c("Socialisation", "Support", "Mediation", "Negotiation",
                             "Adjudication", "Investigation", "Info facilitation")) + 
  labs(x="No. of cases", y="", title="(b) Direct supplier process") +
  theme(title= element_text(size=10))
# Figure 4c. focal company resolution action
plot_focal_res <- 
  table_analysis %>%
  melt(measure.vars=string_Focal_resolution, variable.name='Focal_resolution_action') %>%
  filter(value=='1') %>% 
  ggplot(aes(y= fct_rev(fct_infreq(Focal_resolution_action)))) + 
  geom_bar(fill= '#1F78B4') +
  scale_y_discrete(labels= c("Improve practice", "Remedial", "Coop SCM", "Re-entry", "Coer SCM")) +
  labs(x="No. of cases", y= "", title="(c) Focal company resolution action") +
  theme(title= element_text(size=10))
# Figure 4d. direct supplier resolution action
plot_sup_res <- 
  table_analysis %>%
  melt(measure.vars=string_Supplier_resolution, variable.name="Supplier_resolution") %>% 
  filter(value=="1") %>%
  ggplot(aes(y=fct_rev(fct_infreq(Supplier_resolution)))) + 
  geom_bar(fill= '#1F78B4') +
  scale_y_discrete(labels= c("SSCM", "Improve practice", "Policy", "Remedial")) +
  labs(x='No. of cases', y='', title='(d) Direct supplier resolution action') +
  theme(title= element_text(size=10))
# Combined Figure 4
ggarrange(plot_focal_process, plot_sup_process, plot_focal_res, plot_sup_res, 
          ncol=2, nrow=2, align='hv')

# Figure 5, no. of cases related to engagement nature
table_analysis %>% 
  ggplot(aes(y= fct_rev(fct_infreq(Engagement_nature)))) + 
  geom_bar(fill= "#1F78B4") + 
  labs(x= "No. of cases", y= "Engagement nature") +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size=11.5),
        legend.title = element_text(size=12), legend.text = element_text(size=11.5))
#####
# Code for additional info
# Freq table, focal process
table_analysis %>% 
  melt(measure.vars=string_Focal_process, variable.name="Focal_process") %>% filter(value=="1") %>% 
  group_by(Focal_process) %>% summarise(Count = n()) %>%
  mutate(Percent = round((Count/sum(Count)*100), digits = 1)) %>% 
  write.csv(quote=F, row.names=F)
# Freq table, focal resolution action
table_analysis %>% 
  melt(measure.vars=string_Focal_resolution, variable.name='Focal_resolution_action') %>%
  filter(value == "1") %>% group_by(Focal_resolution_action) %>% 
  summarise(Count = n()) %>% 
  mutate(Percent = round((Count/sum(Count)*100), digits = 1)) %>%
  arrange(-Count)
# Freq table, supplier process
table_analysis %>%
  melt(measure.vars=string_Supplier_process, variable.name="Supplier_process") %>% 
  filter(value=="1") %>% group_by(Supplier_process) %>%
  summarise(Count= n()) %>% 
  mutate(Percent = round((Count/sum(Count)*100), digits = 1)) %>% 
  arrange(Count) %>% write.csv(quote = F, row.names=F)
# Freq table, supplier resolution action
table_analysis %>%
  melt(measure.vars=string_Supplier_resolution, variable.name="Supplier_resolution") %>% 
  filter(value=="1") %>% group_by(Supplier_resolution) %>%
  summarise(Count= n()) %>% 
  mutate(Percent = round((Count/sum(Count)*100), digits = 1)) %>% 
  arrange(Count) %>% write.csv(quote = F, row.names=F)
# Freq table
table_analysis %>% 
  filter(Company != "NA") %>%
  group_by(Engagement_nature) %>% summarise(Count = n()) %>%
  mutate(Percent = round((Count/sum(Count)*100), digits = 1)) %>% 
  arrange(Count) %>% write.csv(quote=F, row.names=F)