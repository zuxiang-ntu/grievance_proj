## 3.7 Complexity and characteristics vs. Duration

# Figure 9a. Complexity vs duration
# Stats table object to plot median and mean
stats_complexity_dur <- table_analysis %>% 
  filter(Status=="Closed", Complexity != "NA") %>%
  group_by(Complexity) %>% 
  summarise(Median = round((median(Duration_lodged_closed, na.rm = T)), digits=1),
            Mean = round((mean(Duration_lodged_closed, na.rm = T)), digits=1)) %>%
  melt(id.vars="Complexity")
# Figure
plot_complexity_duration <- 
  table_analysis %>% 
  filter(Status=="Closed", Complexity != "NA") %>%
  ggplot(aes(x = Complexity, y = Duration_lodged_closed)) + 
  geom_jitter(position = position_jitter(0.2, 0.2), alpha=0.2) +
  geom_line(data= stats_complexity_dur,
            aes(x=Complexity, y=value, linetype=variable), size=1) + 
  scale_x_continuous(n.breaks= 8) +
  labs(x = "", y = "Duration (months)", linetype="", title= "(a)") +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size=11.5))
# sample size
table_analysis %>% 
  filter(Status=="Closed", Complexity != "NA") %>%
  select(Complexity, Duration_lodged_closed) %>% 
  na.omit() %>% nrow()
# Regression stats
lm(Duration_lodged_closed ~ Complexity, 
   table_analysis %>% 
     filter(Status=="Closed")) %>% 
  summary()

# Figure 9b. Allegation clarity vs duration
# Stats table to plot median and mean
stats_clarity_dur <-
  table_analysis %>% 
  filter(Status=="Closed", GC_clarity != "NA") %>%
  group_by(GC_clarity) %>% 
  summarise(Median = round((median(Duration_lodged_closed, na.rm = T)), digits=1),
            Mean = round((mean(Duration_lodged_closed, na.rm = T)), digits=1)) %>%
  melt(id.vars="GC_clarity")
# Figure
plot_clarity_duration <-
  table_analysis %>% 
  filter(Status=="Closed", GC_clarity != "NA") %>%
  ggplot(aes(x= GC_clarity, y = Duration_lodged_closed)) + 
  geom_jitter(position = position_jitter(0.2, 0.2), alpha=0.2) +
  geom_line(data= stats_clarity_dur, 
            aes(x= GC_clarity, y= value, group= variable, linetype= variable),
            size= 1) +
  labs(x= "", y= "Duratiion (months)", linetype= "", title= "(b)") +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size=11.5))
# sample size
table_analysis %>% 
  filter(Status=="Closed", GC_clarity != "NA") %>% 
  select(GC_clarity, Duration_lodged_closed) %>%
  na.omit() %>% nrow()
# Regression stats
lm(Duration_lodged_closed ~ GC_clarity, 
   table_analysis %>% filter(Status=="Closed", GC_clarity != "NA")) %>% 
  summary() 

# Figure 9c. Status of grievance raiser vs Duration
# Stats table to plot median and mean
stats_status_dur <- 
  table_analysis %>% 
  filter(Status=="Closed", GC_status != "NA") %>% 
  mutate(GC_status = factor(GC_status, levels=c("Internal reporting", "High profile", "Low profile"))) %>%
  group_by(GC_status) %>% 
  summarise(Median = round((median(Duration_lodged_closed, na.rm = T)), digits=1),
            Mean = round((mean(Duration_lodged_closed, na.rm = T)), digits=1)) %>% 
  melt(id.vars="GC_status")
# Figure
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
  labs(x= "", y= "Duratiion (months)", linetype= "", title= "(c)") +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size=11.5))
# sample size
table_analysis %>% 
  filter(Status=="Closed", GC_status != "NA") %>% 
  select(GC_status, Duration_lodged_closed) %>% 
  na.omit() %>% nrow()
# Regression stats
lm(Duration_lodged_closed ~ GC_status, 
   table_analysis %>% 
     mutate(GC_status = factor(GC_status, levels=c("Internal reporting", "High profile", "Low profile")))) %>% 
  summary()

# Figure 9d. Number of aggrieved parties vs Duration
# Stats table to plot median and mean
stats_number_dur <-
  table_analysis %>% 
  filter(Status=="Closed", !GC_number %in% c("4", "NA")) %>%
  group_by(GC_number) %>%  
  summarise(Median = round((median(Duration_lodged_closed, na.rm = T)), digits=1),
            Mean = round((mean(Duration_lodged_closed, na.rm = T)), digits=1)) %>% 
  melt(id.vars="GC_number")
# Figure 9d
plot_aggrieved_duration <-
  table_analysis %>% 
  filter(Status=="Closed", !GC_number %in% c("4", "NA")) %>%
  ggplot(aes(x = as.factor(GC_number), y = Duration_lodged_closed)) + 
  geom_jitter(position = position_jitter(0.2, 0.2), alpha=0.2) +
  geom_line(data= stats_number_dur, 
            aes(x= GC_number, y= value, group= variable, linetype= variable),
            size= 1) +
  labs(x= "", y= "Duratiion (months)", linetype= "", title= "(d)") +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size=11.5))
# sample size
table_analysis %>% 
  filter(Status=="Closed") %>%
  select(GC_number, Duration_lodged_closed) %>%
  na.omit() %>% nrow()
# Regression stats
lm(Duration_lodged_closed ~ GC_number,
   table_analysis %>% 
     filter(Status=="Closed", !GC_number %in% c("4", "NA"))) %>% 
  summary()

# Figure 9e. Sourcing relationship vs Duration
# Stats table to plot median and mean
stats_sourcing_dur <- 
  table_analysis %>% 
  filter(Status=="Closed", !GC_sourcing %in% c("NA", "Unable to determine")) %>%
  mutate(GC_sourcing = factor(GC_sourcing, levels = c("Not in supply chain", "Vertical", "Direct", "Indirect"))) %>%
  group_by(GC_sourcing) %>%
  summarise(Median = round((median(Duration_lodged_closed, na.rm = T)), digits=1),
            Mean = round((mean(Duration_lodged_closed, na.rm = T)), digits=1)) %>%
  melt(id.vars="GC_sourcing")
# Figure
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
  scale_x_discrete(labels=c("Non-supplier", "Vertical", "Direct", "Indirect")) +
  labs(x= "", y= "Duratiion (months)", linetype= "", title= "(e)") +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size=11.5))
# sample size
table_analysis %>% 
  filter(Status=="Closed", !GC_sourcing %in% c("NA", "Unable to determine")) %>%
  select(GC_sourcing, Duration_lodged_closed) %>% 
  na.omit() %>% nrow()
# Regression stats
lm(Duration_lodged_closed ~ GC_sourcing, 
   table_analysis %>% 
     filter(Status=="Closed", !GC_sourcing %in% c("NA", "Unable to determine")) %>%
     mutate(GC_sourcing = factor(GC_sourcing, levels = c("Not in supply chain", "Vertical", "Direct", "Indirect")))) %>% 
  summary()

# Figure 9f. Grievance theme vs Duration
# Stats table to plot median and mean
stats_theme_dur <- 
  table_analysis %>% 
  filter(Status=="Closed") %>%
  mutate(Grievance_theme = factor(Grievance_theme, levels = c("Environmental", "Social", "Both"))) %>%
  group_by(Grievance_theme) %>%
  summarise(Median = round((median(Duration_lodged_closed, na.rm = T)), digits=1),
            Mean = round((mean(Duration_lodged_closed, na.rm = T)), digits=1)) %>%
  melt(id.vars="Grievance_theme")
# Figure
plot_theme_duration <-
  table_analysis %>% 
  filter(Status=="Closed") %>%
  mutate(Grievance_theme = factor(Grievance_theme, levels = c("Environmental", "Social", "Both"))) %>%
  ggplot(aes(x = Grievance_theme, y = Duration_lodged_closed)) + 
  geom_jitter(position = position_jitter(0.2, 0.2), alpha=0.2) +
  geom_line(data= stats_theme_dur, 
            aes(x= Grievance_theme, y= value, group= variable, linetype= variable),
            size= 1) +
  labs(x= "", y= "Duratiion (months)", linetype= "", title= "(f)") +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size=11.5))
# sample size
table_analysis %>% 
  filter(Status=="Closed") %>%
  select(Grievance_theme, Duration_lodged_closed) %>% 
  na.omit() %>% nrow()
# Regression stats
lm(Duration_lodged_closed ~ Grievance_theme, 
   table_analysis %>% 
     filter(Status=="Closed") %>%
     mutate(Grievance_theme = factor(Grievance_theme, levels = c("Environmental", "Social", "Both")))) %>% 
  summary()

# Combined Figure 9 plot
ggarrange(plot_complexity_duration, plot_clarity_duration, plot_status_duration, 
          plot_aggrieved_duration, plot_sourcing_duration, plot_theme_duration,
          ncol=2, nrow=3, common.legend = T)
#####
# Code for additional info
# Alt plot complexity v. duration, fitted linear regression line
table_analysis %>% 
  filter(Status=="Closed", Complexity != "NA") %>%
  ggplot(aes(x = Complexity, y = Duration_lodged_closed)) + 
  geom_jitter(position = position_jitter(0.2, 0.2), alpha=0.2) +
  geom_smooth(method= "lm", se= F, color="red", size= 1) +
  scale_x_continuous(n.breaks= 6) +
  scale_color_discrete(labels= "Regression line") +
  labs(x= "Complexity score", y="Duration (months)", title="(b)") +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size=11.5))
# Residuals plot
model <- lm(Duration_lodged_closed ~ Complexity, table_analysis)
summary(model)
model %>% 
  ggplot(aes(x= .fitted, y= .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(x= "Fitted duration values", y= "Residuals", title= "(c)") +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size=11.5))