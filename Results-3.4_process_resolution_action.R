## 3.4 Grievance processes, resolution actions, and supplier engagement  

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

# Figure 6. Proportion of focal company process and resolution actions. Proportion of supplier process and resolution actions
# Data objects for plot legend
cols_foc_process <- setNames(brewer.pal(7, "Paired"), string_Focal_process)
cols_sup_process <- setNames(brewer.pal(7, "Paired"), string_Supplier_process)
# Focal process figure
plot_focal_process <- 
  table_analysis %>% 
  melt(measure.vars=string_Focal_process, variable.name="Focal_process") %>% filter(value=="1") %>% 
  ggplot(aes(x = "", fill = fct_rev(fct_infreq(Focal_process)))) + geom_bar(position="fill") + 
  scale_y_continuous(labels= scales::percent) +
  scale_fill_manual(values=cols_foc_process[-3], labels= c("Information facilitation", "Negotiation", "Investigation", 
                                                           "Adjudication", "Socialisation", "Support")) +
  labs(x="", y="Proportion", fill="Process", title="(a) Focal company process")
# Focal resolution action figure
plot_focal_res <- 
  table_analysis %>%
  melt(measure.vars=string_Focal_resolution, variable.name='Focal_resolution_action') %>%
  filter(value=='1') %>% 
  ggplot(aes(x="", fill=fct_rev(fct_infreq(Focal_resolution_action)))) + geom_bar(position="fill") +
  scale_y_continuous(labels=scales::percent) +
  scale_fill_manual(values= brewer.pal(5, "Dark2"), labels=c("Improve company practice", "Remedial",
                                                             "Coop SCM", "Re-entry", "Coer SCM")) +
  labs(x="", y="Proportion", fill="Resolution action", title="(b) Focal company resolution action")
# Supplier process figure
plot_sup_process <-
  table_analysis %>%
  melt(measure.vars=string_Supplier_process, variable.name="Supplier_process") %>% 
  filter(value=="1") %>%
  ggplot(aes(x="", fill=fct_rev(fct_infreq(Supplier_process)))) + geom_bar(position="fill") +
  scale_y_continuous(labels=scales::percent) +
  scale_fill_manual(values= cols_sup_process, labels=c("Information facilitation", "Negotiation", "Mediation", "Investigation", 
                                                       "Adjudication", "Socialisation", "Support")) +
  labs(x="", y="Proportion", fill="Process", title="(c) Supplier process")
# Supplier resolution action figure
plot_sup_res <- 
  table_analysis %>%
  melt(measure.vars=string_Supplier_resolution, variable.name="Supplier_resolution") %>% 
  filter(value=="1") %>%
  ggplot(aes(x="", fill=fct_rev(fct_infreq(Supplier_resolution)))) + geom_bar(position="fill") +
  scale_y_continuous(labels=scales::percent) +
  scale_fill_manual(values=brewer.pal(4, "Set2"), labels=c("Supply chain management", "Improve company practice", 
                                                           "Policy", "Remedial")) +
  labs(x='', y='Proportion', fill='Resolution action', title='(d) Supplier resolution action')
# Combined Figure 6 
ggarrange(plot_focal_process, plot_focal_res, plot_sup_process, plot_sup_res, 
          ncol=2, nrow=2)

# Focal company supplier engagement
# Freq table
table_analysis %>% 
  filter(Engagement_nature != "NA") %>% 
  group_by(Engagement_nature) %>% summarise(Count = n()) %>%
  mutate(Percent = round((Count/sum(Count)*100), digits = 1)) %>% 
  write.csv(quote=F, row.names=F)
