## Getting R session and data table ready
#################
.libPaths("C:\\Users\\ASEuser\\OneDrive - Nanyang Technological University\\NTU RA\\Grievance Database-Project docs\\Analysis\\R\\win-library")
library(openxlsx)
library(readxl)
library(rlang)
library(tidyverse)
library(readr)
library(summarytools)
library(lubridate)
library(reshape2)
library(janitor)
library(vcd)
library(sjPlot)
library(sjstats)
## Create table for analysis
# Import database excel
mydata <- read_excel("C:\\Users\\ASEuser\\OneDrive - Nanyang Technological University\\NTU RA\\Grievance Database-Project docs\\GrievanceProcedure_v2_20220928.xlsx")
# 1.2 Create base table
table_analysis <- mydata %>%
  transmute(ID = UID, Company = COMPANY, Status = STATUS, Country = COUNTRY, Grievance_raiser = `GRIEVANCE RAISER`, GR_type = `GRIEVANCE RAISER TYPE`,
            Sourcing_rs = `SOURCING RELATION`, Acc_foc_rs = `ACCUSED TO FOCAL RELATIONSHIP`, Int_foc_rs = `INTERMEDIATE COMPANY RELATIONSHIP`,
            Engagement_channel = mydata$`SUPPLIER ENGAGEMENT CHANNEL`, Channel_direct = mydata$`CHANNEL-DIRECT`, 
            Channel_indirect = mydata$`CHANNEL-INDIRECT`, Channel_strategic_alliance = mydata$`CHANNEL-STRATEGIC_ALLIANCE`, 
            Channel_third_party = mydata$`CHANNEL-THRID_PARTY`, Engagement_nature = mydata$`SUPPLIER ENGAGEMENT NATURE`,
            Grievance_theme = `GRIEVANCE THEME`,
            ST_deforestation = `SUB-THEME DEFORESTATION`, ST_peat_development = `SUB-THEME PEAT DEVELOPMENT`, ST_fire = `SUB-THEME FIRE`,
            ST_biodiversity = `SUB-THEME BIODIVERSITY`, ST_pollution = `SUB-THEME POLLUTION`, ST_labour = `SUB-THEME LABOUR`,
            ST_land = `SUB-THEME LAND`, ST_human_rights = `SUB-THEME HUMAN RIGHTS ABUSE`, ST_community = `SUB-THEME COMMUNITY`,
            ST_corruption = `SUB-THEME CORRUPTION`,  # SUb-theme end
            IS_deforestation = `ISSUE DEFORESTATION`, IS_source_conflict_oil = `ISSUE SOURCING CONFLICT PALM OIL`,
            IS_deforestation_prep = `ISSUE DEFORESTATION PREPARATION`, IS_peat_development = `ISSUE PEAT DEVELOPMENT`,
            IS_forest_burning = `ISSUE FOREST BURNING`, IS_peatland_burning = `ISSUE PEATLAND BURNING`, IS_forest_fire = `ISSUE FOREST FIRE`,
            IS_habitat_loss = `ISSUE HABITAT LOSS`, IS_wildlife_threat = `ISSUE WILDLIFE THREAT`, IS_pollution = `ISSUE POLLUTION OF WATER SOURCE`,
            IS_wage_remuneration = `ISSUE WAGE REMUNERATION`, IS_health_safety = `ISSUE HEALTH SAFETY`, IS_social_security = `ISSUE SOCIAL SECURITY`,
            IS_employment_security = `ISSUE EMPLOYMENT SECURITY`, IS_freedom_association = `ISSUE FREEDOM OF ASSOCIATION`,
            IS_forced_labour = `ISSUE FORCED LABOUR`, IS_child_labour = `ISSUE CHILD LABOUR`, 
            IS_land_grabbing = `ISSUE LAND GRABBING`, IS_land_contestation = `ISSUE LAND CONTESTATION`, IS_illegal_land_use = `ISSUE ILLEGAL LAND USE`,
            IS_intimidation = `ISSUE INTIMIDATION`, IS_violence = `ISSUE VIOLENCE`, IS_eviction = `ISSUE EVICTION`,
            IS_FPIC = `ISSUE FPIC`, IS_smallholder = `ISSUE SMALLHOLDER`, IS_community_development = `ISSUE COMMUNITY DEVELOPMENT`,
            IS_bribery = `ISSUE BRIBERY`, IS_tax_evasion = `ISSUE TAX EVASION`, IS_obstruction_of_justice = `ISSUE OBSTRUCTION OF JUSTICE`, # Issue end
            FP_info_facil = `FP-INFORMATION FACILITATION`, FP_negotiation = `FP-NEGOTIATION`, FP_mediation = `FP-MEDIATION`,
            FP_investigation = `FP-INVESTIGATION`, FP_adjudication = `FP-ADJUDICATION`, FP_socialisation = `FP-SOCIALISATION`,
            FP_support = `FP-SUPPORT`, # Focal process end
            Focal_resolution_type = `FOCAL COMPANY RESOLUTION TYPE`, Focal_resolution = `FOCAL COMPANY RESOLUTION`,
            T1_rehab_land = `T1 REHABILITATE LAND`, T1_compensation = `T1 COMPENSATION`, T1_business_pressure = `T1 BUSINESS PRESSURE`, 
            T1_terminate_business = `T1 TERMINATE BUSINESS`, T1_re_entry = `T1 RE-ENTRY`, T1_blacklist = `T1 BLACKLIST COMPANY`,
            T1_supplier_support = `T1 SUPPLIER SUPPORT PROGRAMME`, T1_improve_human_rights = `T1 IMPROVE HUMAN RIGHTS PRACTICE`,
            T1_improve_labour = `T1 IMPROVE LABOUR PRACTICE`, # T1 outcome end
            SP_info_facil = `SP-INFORMATION FACILITATION`, SP_negotiation = `SP-NEGOTIATION`, SP_mediation = `SP-MEDIATION`,
            SP_investigation = `SP-INVESTIGATION`, SP_adjudication = `SP-ADJUDICATION`, SP_socialisation = `SP-SOCIALISATION`,
            SP_support = `SP-SUPPORT`, # Supplier process end
            Supplier_resolution_type = `SUPPLIER RESOLUTION TYPE`,
            T2_stop_harm = `T2 STOP HARM`, T2_land_assessment = `T2 CONDUCT LAND ASSESSMENT`, T2_corrective_action = `T2 CORRECTIVE ACTION`,
            T2_compensation = `T2 COMPENSATION`, T2_rehab_land = `T2 REHABILITATE LAND`, T2_adopt_policy = `T2 ADOPT POLICY`,
            T2_develop_policy = `T2 DEVELOP POLICY`, T2_enhance_policy = `T2 ENHANCE POLICY`, T2_improve_env_prac = `T2 IMPROVE ENVIRONMENTAL SUSTAINABILITY PRACTICE`,
            T2_improve_sourcing = `T2 IMPROVE SOURCING PRACTICE`, T2_improve_fire_mgmt = `T2 IMPROVE FIRE MANAGEMENT PRACTICE`,
            T2_improve_human_rights = `T2 IMPROVE HUMAN RIGHTS PRACTICE`, T2_improve_labour = `T2 IMPROVE LABOUR PRACTICE`,
            T2_terminate_business = `T2 TERMINATE BUSINESS`, T2_blacklist = `T2 BLACKLIST COMPANY`, T2_re_entry = `T2 RE-ENTRY`,
            T2_action_plan = `T2 IMPLEMENT ACTION PLAN`, T2_traceability = `T2 ACHIEVE TRACEABILITY`,
            T2_community_engagement = `T2 COMMUNITY ENGAGEMENT`, # T2 outcome end
            GC_clarity = `CLARITY OF ALLEGATION`, GC_status = `STATUS OF GRIEVANCE RAISER`, GC_number = `NUMBER OF AGGRIEVED PARTIES`,
            GC_sourcing = `SOURCING RELATIONSHIP`, Complexity = `COMPLEXITY SCORE`, Grievance_outcome = `GRIEVANCE OUTCOME`) %>%
  mutate(Grievance_outcome = factor(Grievance_outcome, levels=c("Nullified", "Monitor", "Direct action", "Policy compliance",
                                                                "Supplier support", "Multiplier", "Suspend", "Re-entry")),
         Sourcing_rs= factor(Sourcing_rs,levels = c("Vertical-1", "Vertical-2", "Direct-1", "Direct-2", "Indirect", 
                                        "Unable to determine", "Not in supply chain", "NA")))
# Date as numeric
Date_lodged <- excel_numeric_to_date(as.numeric(mydata$`DATE LODGED`))
Date_closed <- excel_numeric_to_date(as.numeric(mydata$`DATE CLOSED`))
Date_last_purchase <- excel_numeric_to_date(as.numeric(mydata$`DATE PURCHASE`))
Date_reentry <- excel_numeric_to_date(as.numeric(mydata$`DATE ENTRY`))
Date_first_engagement <- excel_numeric_to_date(as.numeric(mydata$`DATE ENGAGEMENT`))

# Duration columns
table_analysis <- table_analysis %>% mutate(Duration_lodged_closed = time_length(interval(Date_lodged, Date_closed), unit="months"),
                                            Duration_lastpurchase_reentry = time_length(interval(Date_last_purchase, Date_reentry), unit="months"),
                                            Duration_first_engagement = as.numeric(Date_first_engagement - Date_lodged)) %>%
  relocate(c(Duration_lodged_closed, Duration_lastpurchase_reentry, Duration_first_engagement), .after = Country)
table_analysis$Duration_lodged_closed <- replace(table_analysis$Duration_lodged_closed, which(table_analysis$Duration_lodged_closed < 0), NA)
table_analysis$Duration_lastpurchase_reentry <- replace(table_analysis$Duration_lastpurchase_reentry, which(table_analysis$Duration_lastpurchase_reentry < 0), NA)
table_analysis$Duration_first_engagement <- replace(table_analysis$Duration_first_engagement, which(table_analysis$Duration_first_engagement < 0), NA)

# Data string for melting to long format
string_subtheme <- c('ST_deforestation', 'ST_peat_development', 'ST_fire',
                  'ST_biodiversity', 'ST_pollution', 'ST_labour',
                  'ST_land', 'ST_human_rights', 'ST_community', 'ST_corruption')
string_issue <- c('IS_deforestation', 'IS_source_conflict_oil','IS_deforestation_prep','IS_peat_development',
                  'IS_forest_burning', 'IS_peatland_burning', 'IS_forest_fire',
                  'IS_habitat_loss', 'IS_wildlife_threat', 'IS_pollution',
                  'IS_wage_remuneration', 'IS_health_safety', 'IS_social_security', 'IS_employment_security',
                  'IS_freedom_association', 'IS_forced_labour', 'IS_child_labour',
                  'IS_land_grabbing', 'IS_land_contestation', 'IS_illegal_land_use',
                  'IS_intimidation', 'IS_violence', 'IS_eviction', 
                  'IS_FPIC', 'IS_smallholder', 'IS_community_development',
                  'IS_bribery', 'IS_tax_evasion', 'IS_obstruction_of_justice')
string_Focal_process <- c('FP_info_facil', 'FP_negotiation', 'FP_mediation','FP_investigation',
                       'FP_adjudication', 'FP_socialisation', 'FP_support')
string_T1_outcome <- c('T1_rehab_land', 'T1_compensation', 'T1_business_pressure', 'T1_terminate_business',
                       'T1_re_entry', 'T1_blacklist', 'T1_supplier_support', 'T1_improve_human_rights', 
                       'T1_improve_labour')
string_Supplier_process <- c('SP_info_facil', 'SP_negotiation', 'SP_mediation', 'SP_investigation',
                       'SP_adjudication', 'SP_socialisation', 'SP_support')
string_T2_outcome <- c('T2_stop_harm', 'T2_land_assessment', 'T2_corrective_action', 'T2_compensation',
                       'T2_rehab_land', 'T2_adopt_policy', 'T2_develop_policy', 'T2_enhance_policy',
                       'T2_improve_env_prac', 'T2_improve_sourcing', 'T2_improve_fire_mgmt', 'T2_improve_human_rights',
                       'T2_improve_labour', 'T2_terminate_business', 'T2_blacklist', 'T2_re_entry',
                       'T2_action_plan', 'T2_traceability', 'T2_community_engagement')

#################
## Descriptive results
# 1.0 Background
# Number of cases by company
table_analysis %>% filter(Company != "NA") %>%
  freq(Company) %>% write.csv(quote = F)
# Number of cases by status
table_analysis %>% filter(Status != "NA") %>%
  freq(Status) %>% write.csv(quote = F)
# Number of cases by Country
table_analysis %>% filter(Country != "NA") %>%
  freq(Country, order="freq") %>% write.csv(quote=F)
# Number of cases by grievance raiser
table_analysis %>% filter(GR_type != "NA") %>%
  freq(GR_type, order="freq") %>% write.csv(quote=F)
table_analysis %>% filter(GR_type %in% "NGO", Grievance_raiser != "NA") %>%
  freq(Grievance_raiser, order="freq") %>% write.csv(quote=F)

# 2.0 Duration
# Summary stats
table_analysis %>%
  filter(Status == "Closed", Duration_lodged_closed != "NA") %>%
  summarise(count = n(), min = min(Duration_lodged_closed, na.rm = T), 
            quant1 = quantile(Duration_lodged_closed, probs = 0.25, na.rm = T),
            med = median(Duration_lodged_closed, na.rm = T), mean = mean(Duration_lodged_closed, na.rm = T), 
            quant3 = quantile(Duration_lodged_closed, probs = 0.75, na.rm = T), max = max(Duration_lodged_closed, na.rm = T)) %>%
  write.csv(quote=F, row.names = F)
# Boxplot 
table_analysis %>% filter(Status == "Closed", Duration_lodged_closed != "NA") %>%
  ggplot(aes(x = Duration_lodged_closed)) + geom_boxplot() + 
  labs(x = "Months", title = "Duration of Grievance Resolution")
# Histogram
table_analysis %>% filter(Status == "Closed", Duration_lodged_closed != "NA") %>%
  ggplot(aes(x = Duration_lodged_closed)) + geom_histogram(binwidth = 3) + 
  labs(x = "Months", y = "Count", title = "Duration of Grievance Resolution")

# 3.1 Theme
# Stats
table_analysis %>% filter(Grievance_theme != "NA") %>%
  freq(Grievance_theme) %>% write.csv(quote = F)
# Bar graph
table_analysis %>% filter(Grievance_theme != "NA") %>%
  ggplot(aes(x=Grievance_theme, fill=Grievance_theme)) + geom_bar() +
  labs(x="Grievance theme", y="Count", fill="Grievance theme", title="Count of Grievance themes")

# 3.2 Sub-Theme
# 3.2.1 All themes
# Stats
table_analysis %>% melt(measure.vars = string_subtheme, variable.name='Sub_theme') %>% 
  filter(value=="1") %>% freq(Sub_theme) %>% write.csv(quote=F)
# Bar graph
table_analysis %>% melt(measure.vars = string_subtheme, variable.name='Sub_theme') %>% 
  filter(value=="1") %>% ggplot(aes(y = fct_rev(Sub_theme))) + geom_bar() + labs(y = 'Sub theme', title="Count of sub themes")
# 3.2.2 Environmental theme
# Stats
table_analysis %>% melt(measure.vars = string_subtheme, variable.name='Sub_theme') %>% 
  filter(value=="1", Grievance_theme == "Environmental") %>% freq(Sub_theme) %>% write.csv(quote=F)
# Bar graph
table_analysis %>% melt(measure.vars = string_subtheme, variable.name='Sub_theme') %>% 
  filter(value=="1", Grievance_theme == "Environmental") %>%
  ggplot(aes(y = fct_rev(Sub_theme), fill=Sub_theme)) + geom_bar() + 
  labs(y = 'Sub theme', fill="Sub theme", title="Sub themes (Environmental)") + theme(legend.position = "none")
#3.2.3 Social theme
# Stats
table_analysis %>% melt(measure.vars = string_subtheme, variable.name='Sub_theme') %>% 
  filter(value=="1", Grievance_theme == "Social") %>% freq(Sub_theme) %>% write.csv(quote=F)
# Bar graph
table_analysis %>% melt(measure.vars = string_subtheme, variable.name='Sub_theme') %>% 
  filter(value=="1", Grievance_theme == "Social") %>%
  ggplot(aes(y = fct_rev(Sub_theme), fill=Sub_theme)) + geom_bar() + labs(y = 'Sub theme', fill="Sub theme", title="Sub themes (Social)") +
  theme(legend.position = "none")
#3.2.3 Both theme
# Stats
table_analysis %>% melt(measure.vars = string_subtheme, variable.name='Sub_theme') %>% 
  filter(value=="1", Grievance_theme == "Both") %>% freq(Sub_theme) %>% write.csv(quote=F)
# Bar graph
table_analysis %>% melt(measure.vars = string_subtheme, variable.name='Sub_theme') %>% 
  filter(value=="1", Grievance_theme == "Both") %>%
  ggplot(aes(y = fct_rev(Sub_theme), fill=Sub_theme)) + geom_bar() + labs(y = 'Sub theme', title="Sub themes (Both)") + 
  theme(legend.position = "none")

# 3.3 Issues
# 3.3.1 All theme
# Stats
table_analysis %>%
  melt(measure.vars = string_issue, variable.name="Issue") %>%
  filter(value == "1") %>% freq(Issue) %>% write.csv(quote=F)
# Bar graph
table_analysis %>%
  melt(measure.vars = string_issue, variable.name="Issue") %>%
  filter(value == "1") %>% ggplot(aes(y=fct_rev(Issue))) + geom_bar() +
  labs(x = "Count", y="Issue", title="Count of Issues")
# 3.3.2 Environmental theme
# Stats
table_analysis %>%
  melt(measure.vars = string_issue, variable.name="Issue") %>%
  filter(value == "1", Grievance_theme == "Environmental") %>% freq(Issue) %>% write.csv(quote=F)
# Bar graph
table_analysis %>%
  melt(measure.vars = string_issue, variable.name="Issue") %>%
  filter(value == "1", Grievance_theme=="Environmental") %>% ggplot(aes(y=fct_rev(Issue))) + geom_bar() +
  labs(x = "Count", y="Issue", title="Issues (Environmental")
# 3.3.3 Social theme
# Stats
table_analysis %>%
  melt(measure.vars = string_issue, variable.name="Issue") %>%
  filter(value == "1", Grievance_theme == "Social") %>% freq(Issue) %>% write.csv(quote=F)
# Bar graph
table_analysis %>%
  melt(measure.vars = string_issue, variable.name="Issue") %>%
  filter(value == "1", Grievance_theme=="Social") %>% ggplot(aes(y=fct_rev(Issue))) + geom_bar() +
  labs(x = "Count", y="Issue", title="Issues (Social")
# 3.3.4 Both theme
# Stats
table_analysis %>%
  melt(measure.vars = string_issue, variable.name="Issue") %>%
  filter(value == "1", Grievance_theme == "Both") %>% freq(Issue) %>% write.csv(quote=F)
# Bar graph
table_analysis %>%
  melt(measure.vars = string_issue, variable.name="Issue") %>%
  filter(value == "1", Grievance_theme=="Both") %>% ggplot(aes(y=fct_rev(Issue))) + geom_bar() +
  labs(x = "Count", y="Issue", title="Issues (Both")

# 4.0 Sourcing relationship
# Stats
table_analysis %>% filter(GC_sourcing != "NA") %>%
  freq(GC_sourcing) %>% write.csv(quote=F)
# Bar graph
table_analysis %>% filter(!GC_sourcing %in% c('Unable to determine', 'Not in supply chain', 'NA')) %>%
  ggplot(aes(x = GC_sourcing, fill=GC_sourcing)) + geom_bar() + labs(x="Sourcing relationship") + theme(legend.position = 0)

# 4.1 Sourcing by Engagement channel
# Bar graph of counts
table_analysis %>% melt(measure.vars= c('Channel_direct', 'Channel_indirect', 'Channel_strategic_alliance', 'Channel_third_party'),
                        variable.name = "channel") %>%
  filter(!GC_sourcing %in% c('Unable to determine', 'Not in supply chain', 'NA'), value== "1") %>%
  ggplot(aes(x= GC_sourcing, fill=channel)) + geom_bar() + labs(x="Sourcing relationship", fill="Engagement channel") +
  scale_fill_discrete(labels = c("Direct", "Indirect", "Strategic alliance", "Third party"))
# Bar graph of proportions
table_analysis %>% melt(measure.vars= c('Channel_direct', 'Channel_indirect', 'Channel_strategic_alliance', 'Channel_third_party'),
                        variable.name = "channel") %>%
  filter(!GC_sourcing %in% c('Unable to determine', 'Not in supply chain', 'NA'), value== "1") %>%
  ggplot(aes(x= GC_sourcing, fill=channel)) + geom_bar(position="fill") + labs(x="Sourcing relationship", fill="Engagement channel") +
  scale_fill_discrete(labels = c("Direct", "Indirect", "Strategic alliance", "Third party")) +
  scale_y_continuous(labels = scales::percent) + theme(axis.title.y = element_blank())
# Table of counts
table_analysis %>% melt(measure.vars= c('Channel_direct', 'Channel_indirect', 'Channel_strategic_alliance', 'Channel_third_party'),
                        variable.name = "channel") %>%
  filter(!GC_sourcing %in% c('Unable to determine', 'Not in supply chain', 'NA'), value== "1") %>%
  group_by(GC_sourcing, channel) %>% summarise(count = n()) %>% write.csv(quote=F, row.names = F)

# 5.0 Focal company
# 5.1 Focal company process
# Stats
table_analysis %>% melt(measure.vars=string_Focal_process, variable.name="Focal_process") %>%
  filter(value=="1") %>% freq(Focal_process) %>% write.csv(quote=F)
# Bar graph
table_analysis %>% melt(measure.vars=string_Focal_process, variable.name="Focal_process") %>%
  filter(value=="1") %>% ggplot(aes(y = fct_rev(Focal_process))) + geom_bar() + labs(y="Focal process")
# 5.2 Focal company supplier engagement
# Stats
table_analysis %>% filter(Engagement_nature != "NA") %>% 
  freq(Engagement_nature) %>% write.csv(quote=F)
# Bar graph
table_analysis %>% filter(Engagement_nature != "NA") %>%
  ggplot(aes(x='', fill=Engagement_nature)) + geom_bar(position="fill") + 
  labs(x="Engagement nature", y="", fill="Engagement nature") + scale_y_continuous(labels=scales::percent) +
  scale_fill_discrete(labels=c("Both: 31%", "Coercive: 17%", "Cooperative: 52%"))
# 5.3 Focal company resolution action
# Stats
table_analysis %>% filter(Focal_resolution != "NA") %>%
  freq(Focal_resolution, order = '-freq') %>% write.csv(quote=F)
# Bar graph 
table_analysis %>% filter(Focal_resolution != "NA") %>%
  ggplot(aes(x='', fill= fct_rev(fct_infreq(Focal_resolution)))) + geom_bar(position="fill") +
  labs(x='Focal company resolution action',y='', fill="Resolution action") + scale_y_continuous(labels=scales::percent) +
  scale_fill_discrete(labels=c("Coop-SCM, Coer-SCM: 1%", "Improve company practice: 2%", "Remedial: 3%",
                               "Coop-SCM: 11%", "Re-entry: 20%", "Coer-SCM: 63%"))

# 6.0 Overall grievance outcomes
# Stats
table_analysis %>% filter(!is.na(Grievance_outcome)) %>%
  freq(Grievance_outcome) %>% write.csv(quote=F)
# Bar graph
table_analysis %>% filter(!is.na(Grievance_outcome))  %>%
  ggplot(aes(x='', fill= fct_rev(fct_infreq(Grievance_outcome)))) + geom_bar(position="fill") +
  scale_y_continuous(labels=scales::percent) + labs(x='Overall grievance outcome', y='', fill='') +
  scale_fill_discrete(labels=c('Direct action: 2%', 'Supplier support: 4%', 'Nullified: 5%', 'Policy compliance: 5%',
                            'Re-entry: 7%', 'Multiplier: 9%', 'Suspend: 25%', 'Monitor: 43%'))

# 7.0 Grievance characteristics
# Stats
table_analysis %>% filter(Company != "NA") %>% 
  melt(measure.vars=c("GC_clarity", "GC_status", "GC_number", "GC_sourcing")) %>%
  group_by(variable) %>% count(value) %>% mutate(prop = 100*n/sum(n)) %>% write.csv(quote=F, row.names=F)
# Bar graph
table_analysis %>% filter(Company != "NA") %>% 
  melt(measure.vars=c("GC_clarity", "GC_status", "GC_number", "GC_sourcing")) %>%
  ggplot(aes(x=variable, fill=value)) + geom_bar() + scale_fill_discrete()
# Complexity score
# Stats
table_analysis %>%  
  summarise(min = min(Complexity, na.rm = T), quant1 = quantile(Complexity, probs = 0.25, na.rm = T), 
            med = median(Complexity, na.rm = T), mean = mean(Complexity, na.rm = T),
            quant3 = quantile(Complexity, probs = 0.75, na.rm = T), max = max(Complexity, na.rm = T)) %>%
  write.csv(quote=F, row.names = F)
# Histogram
table_analysis %>% ggplot(aes(x=Complexity)) + geom_histogram(binwidth=1)

table_analysis %>% filter(Grievance_outcome != "NA") %>%
  ggplot(aes(y=Grievance_outcome, x=Complexity)) + geom_point() +geom_jitter()
