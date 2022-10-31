## 3.7 Complexity and characteristics vs. Duration

# Figure 10. Grievance complexity vs Duration
# Stats table object
stats_complexity_dur <- table_analysis %>% 
  filter(Status=="Closed", Complexity != "NA") %>%
  group_by(Complexity) %>% 
  summarise(Median = round((median(Duration_lodged_closed, na.rm = T)), digits=1),
            Mean = round((mean(Duration_lodged_closed, na.rm = T)), digits=1)) %>%
  melt(id.vars="Complexity")
# Figure
table_analysis %>% 
  filter(Status=="Closed", Complexity != "NA") %>%
  ggplot(aes(x = Complexity, y = Duration_lodged_closed)) + 
  geom_jitter(position = position_jitter(0.2, 0.2), alpha=0.2) +
  geom_line(data= stats_complexity_dur,
            aes(x=Complexity, y=value, linetype=variable), size=1) + 
  scale_x_continuous(n.breaks= 6) +
  labs(x = "Complexity score", y = "Duration (months)", linetype="") +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size=11.5))

# Figure 11. Grievance characteristics vs Duration
# Stats table for clarity vs Duration
stats_clarity_dur <-
  table_analysis %>% 
  filter(Status=="Closed", GC_clarity != "NA") %>%
  group_by(GC_clarity) %>% 
  summarise(Median = round((median(Duration_lodged_closed, na.rm = T)), digits=1),
            Mean = round((mean(Duration_lodged_closed, na.rm = T)), digits=1)) %>%
  melt(id.vars="GC_clarity")
# Stats table for Grievance raiser status vs Duration
stats_status_dur <- 
  table_analysis %>% 
  filter(Status=="Closed", GC_status != "NA") %>% 
  mutate(GC_status = factor(GC_status, levels=c("Internal reporting", "High profile", "Low profile"))) %>%
  group_by(GC_status) %>% 
  summarise(Median = round((median(Duration_lodged_closed, na.rm = T)), digits=1),
            Mean = round((mean(Duration_lodged_closed, na.rm = T)), digits=1)) %>% 
  melt(id.vars="GC_status")
# Stats table for number of aggrieved
stats_number_dur <-
  table_analysis %>% 
  filter(Status=="Closed", !GC_number %in% c("4", "NA")) %>%
  group_by(GC_number) %>%  
  summarise(Median = round((median(Duration_lodged_closed, na.rm = T)), digits=1),
            Mean = round((mean(Duration_lodged_closed, na.rm = T)), digits=1)) %>% 
  melt(id.vars="GC_number")
# Stats table for sourcing
stats_sourcing_dur <- 
  table_analysis %>% 
  filter(Status=="Closed", !GC_sourcing %in% c("NA", "Unable to determine")) %>%
  mutate(GC_sourcing = factor(GC_sourcing, levels = c("Not in supply chain", "Vertical", "Direct", "Indirect"))) %>%
  group_by(GC_sourcing) %>%
  summarise(Median = round((median(Duration_lodged_closed, na.rm = T)), digits=1),
            Mean = round((mean(Duration_lodged_closed, na.rm = T)), digits=1)) %>%
  melt(id.vars="GC_sourcing")

# Allegation clarity vs Duration figure 
plot_clarity_duration <-
table_analysis %>% 
  filter(Status=="Closed", GC_clarity != "NA") %>%
  ggplot(aes(x= GC_clarity, y = Duration_lodged_closed)) + 
  geom_jitter(position = position_jitter(0.2, 0.2), alpha=0.2) +
  geom_line(data= stats_clarity_dur, 
            aes(x= GC_clarity, y= value, group= variable, linetype= variable),
            size= 1) +
  labs(x= "", y= "Duratiion (months)", linetype= "", title= "(a)") +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size=11.5))

# Status of grievance raiser vs Duration figure
plot_status_duration <-
table_analysis %>% 
  mutate(GC_status = factor(GC_status, levels=c("Internal reporting", "High profile", "Low profile"))) %>%
  filter(Status=="Closed", GC_status != "NA") %>%
  ggplot(aes(x = GC_status, y = Duration_lodged_closed)) + 
  geom_jitter(position = position_jitter(0.2, 0.2), alpha=0.2) +
  geom_line(data= stats_status_dur, 
            aes(x= GC_status, y= value, group= variable, linetype= variable),
            size= 1) +
  scale_x_discrete(labels= scales::label_wrap(10)) +
  labs(x= "", y= "Duratiion (months)", linetype= "", title= "(b)") +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size=11.5))

# Number of aggrieved parties vs Duration
plot_aggrieved_duration <-
table_analysis %>% 
  filter(Status=="Closed", !GC_number %in% c("4", "NA")) %>%
  ggplot(aes(x = as.factor(GC_number), y = Duration_lodged_closed)) + 
  geom_jitter(position = position_jitter(0.2, 0.2), alpha=0.2) +
  geom_line(data= stats_number_dur, 
            aes(x= GC_number, y= value, group= variable, linetype= variable),
            size= 1) +
  labs(x= "", y= "Duratiion (months)", linetype= "", title= "(c)") +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size=11.5))

# Sourcing relationship vs Duration
plot_sourcing_duration <-
table_analysis %>% 
  filter(Status=="Closed", !GC_sourcing %in% c("NA", "Unable to determine")) %>%
  mutate(GC_sourcing = factor(GC_sourcing, levels = c("Not in supply chain", "Vertical", "Direct", "Indirect"))) %>%
  ggplot(aes(x = GC_sourcing, y = Duration_lodged_closed)) + 
  geom_jitter(position = position_jitter(0.2, 0.2), alpha=0.2) +
  geom_line(data= stats_sourcing_dur, 
            aes(x= GC_sourcing, y= value, group= variable, linetype= variable),
            size= 1) +
  scale_x_discrete(labels= scales::label_wrap(10)) +
  labs(x= "", y= "Duratiion (months)", linetype= "", title= "(d)") +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size=11.5))

# Combined Figure 11 plot
ggarrange(plot_clarity_duration, plot_status_duration, plot_aggrieved_duration, plot_sourcing_duration, 
          ncol=2, nrow=2, common.legend = T, align='hv')
#####
# Alt plot, complexity vs duration
table_analysis %>% 
  filter(Status=="Closed", Complexity != "NA") %>%
  ggplot(aes(x = Complexity, y = Duration_lodged_closed)) + 
  geom_jitter(position = position_jitter(0.2, 0.2), alpha=0.2) +
  geom_point(data= stats_complexity_dur,
             aes(x=Complexity, y=value, color=variable), 
             size=2, show.legend=F) +
  geom_line(data= stats_complexity_dur,
            aes(x=Complexity, y=value, color=variable), size=1) + 
  annotate("text", 
           x= c(3:8)+0.2, y= c(4.8, 5.2, 13.2, 13, 9.9, 38.8)-2, 
           label= c('4.8', '5.2', '13.2', '13', '9.9', '38.8'), color="blue", fontface= "bold", size= 4.5) +
  annotate("text", 
           x=c(2.8, 3.8, 4.8, 5.8, 6.8, 8.2), y=c(8.9, 9.8, 20.5, 17.3, 25.3, 30.3), 
           label= c('6.9', '7.8', '18.5', '15.3', '23.3', '32.3'), color= "orange", fontface= "bold", size= 4.5) +
  scale_color_manual(values=c("blue", "orange")) + 
  scale_x_continuous(n.breaks= 6) +
  labs(x = "Complexity score", y = "Duration (months)", color="") +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size=11.5))
# Alt plot, clarity vs duration
table_analysis %>% 
  filter(Status=="Closed", GC_clarity != "NA") %>%
  ggplot(aes(x= GC_clarity, y = Duration_lodged_closed)) + 
  geom_jitter(position = position_jitter(0.2, 0.2), alpha=0.2) +
  geom_line(data= stats_clarity_dur, 
            aes(x= GC_clarity, y= value, group= variable, linetype= variable),
            size= 1) +
  geom_point(data= stats_clarity_dur %>% filter(variable=="Median"), 
             aes(x= GC_clarity, y= value, color= as.factor(value)),
             size= 2) +
  geom_point(data= stats_clarity_dur %>% filter(variable=="Mean"), 
             aes(x= GC_clarity, y= value, fill=as.factor(value)),
             size= 2, shape= 21) +
  scale_color_manual(values = c("#92C5DE", "#2166AC"), 
                     label= c("Clear: 9.4", "Unclear: 12.5")) +
  scale_fill_manual(values= c("#F4A582", "#B2182B"), 
                    label= c("Clear: 16.1", "Unclear: 21.5")) +
  labs(x = "", y = "Duration (months)", title="(a) Allegation clarity", color="Median", fill="Mean", linetype="") +
  guides(linetype= guide_legend(order=1), color= guide_legend(order=2), fill= guide_legend(order=3)) +
  coord_cartesian(clip = 'off') +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size=11.5))
# Alt plot, status vs duration
table_analysis %>% 
  mutate(GC_status = factor(GC_status, levels=c("Internal reporting", "High profile", "Low profile"))) %>%
  filter(Status=="Closed", GC_status != "NA") %>%
  ggplot(aes(x = GC_status, y = Duration_lodged_closed)) + 
  geom_jitter(position = position_jitter(0.2, 0.2), alpha=0.2) +
  geom_point(data= stats_status_dur %>% filter(variable=="Median"), 
             aes(x= GC_status, y= value, color= as.character(value)),
             size= 2) +
  geom_point(data= stats_status_dur %>% filter(variable=="Mean"), 
             aes(x= GC_status, y= value, fill= as.character(value)),
             size= 2, shape= 21) +
  geom_line(data= stats_status_dur, 
            aes(x= GC_status, y= value, group= variable, linetype= variable),
            size= 1) +
  scale_color_manual(values = c("#2166AC", "#4393C3", "#92C5DE"),
                     breaks = c(29, 11.2, 8.3), 
                     label= c("Internal reporting: 29", "High profile: 11.2", "Low profile: 8.3")) +
  scale_fill_manual(values= c("#F4A582", "#D6604D", "#B2182B"), 
                    breaks= c(23.5, 16.5, 16.5), 
                    label= c("Internal reporting: 23.5", "High profile: 16.5", "Low profile: 16.5")) +
  labs(x="", y="Duration (months)", title="(b) Status of grievance raiser", col="Median", fill= "Mean", linetype="") +
  guides(linetype= guide_legend(order=1), color= guide_legend(order=2), fill= guide_legend(order=3)) +
  coord_cartesian(clip = 'off') +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size=11.5))
# Alt plot, number of aggrieved vs duration
table_analysis %>% 
  filter(Status=="Closed", !GC_number %in% c("4", "NA")) %>%
  ggplot(aes(x = as.factor(GC_number), y = Duration_lodged_closed)) + 
  geom_jitter(position = position_jitter(0.2, 0.2), alpha=0.1) +
  geom_point(data= stats_number_dur %>% filter(variable=="Median"), 
             aes(x= GC_number, y= value, color= as.factor(value)),
             size= 2.5) +
  geom_point(data= stats_number_dur %>% filter(variable=="Mean"),
             aes(x= GC_number, y= value, fill= as.factor(value)),
             size= 2.5, shape= 21) +  
  geom_line(data= stats_number_dur, 
            aes(x= GC_number, y= value, group= variable, linetype= variable),
            size= 1) +
  scale_color_manual(breaks = c(19.8, 10.2, 13.2, 11.2),
                     label= c("0 party: 19.8", "1 party: 10.2", "2 parties: 13.2", "3 parties: 11.2"),
                     values= c("#D1E5F0", "#92C5DE", "#4393C3", "#2166AC")) +
  scale_fill_manual(breaks= c(19.8, 17.3, 13.3, 21.3),
                    labels= c("0 party: 19.8", "1 party: 17.3", "2 parties: 13.3", "3 parties: 21.3"),
                    values= c("#B2182B", "#D6604D", "#F4A582", "#FDDBC7")) +
  guides(linetype= guide_legend(order=1), color= guide_legend(order=2), fill= guide_legend(order=3)) +
  labs(x="", y="Duration (months)", title="(c) Number of aggrieved parties",
       color="Median", fill= "Mean", linetype="") +
  coord_cartesian(clip = 'off') +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size=11.5))
# Alt plot, sourcing vs duration
table_analysis %>% 
  filter(Status=="Closed", !GC_sourcing %in% c("NA", "Unable to determine")) %>%
  mutate(GC_sourcing = factor(GC_sourcing, levels = c("Not in supply chain", "Vertical", "Direct", "Indirect"))) %>%
  ggplot(aes(x = GC_sourcing, y = Duration_lodged_closed)) + 
  geom_jitter(position = position_jitter(0.2, 0.2), alpha=0.1) +
  geom_point(data= stats_sourcing_dur %>% filter(variable=="Median"), 
             aes(x= GC_sourcing, y= value, color= as.factor(value)),
             size= 2) +
  geom_point(data= stats_sourcing_dur %>% filter(variable=="Mean"), 
             aes(x= GC_sourcing, y= value, fill= as.factor(value)),
             size= 2, shape= 21) +
  geom_line(data= stats_sourcing_dur, 
            aes(x= GC_sourcing, y= value, group= variable, linetype= variable),
            size= 1) +
  scale_color_manual(values= c("#D1E5F0", "#92C5DE", "#4393C3", "#2166AC"), 
                     breaks= c(5.1, 2.7, 13.2, 11.4),
                     label= c("Not in supply chain: 5.1", "Vertical: 2.7", "Direct: 13.2", "Indirect: 11.4")) +
  scale_fill_manual(values= c("#B2182B", "#D6604D", "#F4A582", "#FDDBC7"), 
                    breaks= c(7.1, 6.8, 19.2, 19.4),
                    label= c("Not in supply chain: 7.1", "Vertical: 6.8", "Direct: 19.2", "Indirect: 19.4")) +
  guides(linetype= guide_legend(order=1), color= guide_legend(order=2), fill= guide_legend(order=3)) +
  labs(x="", y="Duration (months)", title="(d) Sourcing relationship", col="Median", fill= "Mean", linetype="") +
  coord_cartesian(clip = 'off') +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size=11.5))