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
#################
## Create table for analysis
# Import database excel
mydata <- read_excel("C:\\Users\\ASEuser\\OneDrive - Nanyang Technological University\\NTU RA\\Grievance Database-Project docs\\GrievanceProcedure_v2_20221006.xlsx")
# Create analysis table
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
# Format dates to calculate duration
Date_lodged <- excel_numeric_to_date(as.numeric(mydata$`DATE LODGED`))
Date_closed <- excel_numeric_to_date(as.numeric(mydata$`DATE CLOSED`))
Date_last_purchase <- excel_numeric_to_date(as.numeric(mydata$`DATE LAST PURCHASE`))
Date_reentry <- excel_numeric_to_date(as.numeric(mydata$`DATE RE-ENTRY`))
Date_first_engagement <- excel_numeric_to_date(as.numeric(mydata$`DATE FIRST ENGAGEMENT`))
# Add duration columns to "table_analysis"
table_analysis <- table_analysis %>% mutate(Duration_lodged_closed = time_length(interval(Date_lodged, Date_closed), unit="months"),
                                            Duration_lastpurchase_reentry = time_length(interval(Date_last_purchase, Date_reentry), unit="months"),
                                            Duration_first_engagement = as.numeric(Date_first_engagement - Date_lodged)) %>%
  mutate(Duration_lodged_closed = replace(Duration_lodged_closed, which(Duration_lodged_closed < 0), NA),
         Duration_lastpurchase_reentry = replace(Duration_lastpurchase_reentry, which(Duration_lastpurchase_reentry < 0), NA),
         Duration_first_engagement = replace(Duration_first_engagement, which(Duration_first_engagement < 0), NA)) %>%
  relocate(c(Duration_lodged_closed, Duration_lastpurchase_reentry, Duration_first_engagement), .after = Country)

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
## Note: I chose to convert tables to csv so that further formatting and changes can be made on excel as I am still unfamiliar with 
## how to achieve the desired output entirely in code. 

# 1.0 Background
# Number of sub-cases by company
table_analysis %>% filter(Company != "NA") %>%
  freq(Company) %>% write.csv(quote = F)
# Number of sub-cases by status
table_analysis %>% filter(Status != "NA") %>%
  freq(Status) %>% write.csv(quote = F)
# Number of sub-cases by Country
table_analysis %>% filter(Country != "NA") %>%
  freq(Country, order="freq") %>% write.csv(quote=F)
## Note: number of cases as reported in grievance website was calculated by manually looking at database UID numbers.

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
# Freq table
table_analysis %>% 
  filter(Grievance_theme != "NA") %>%
  group_by(Grievance_theme) %>% summarise(Count = n()) %>%
  mutate(Percent = round((Count/sum(Count)*100), digits = 1)) %>%
  write.csv(quote = F)
# Bar graph
table_analysis %>% filter(Grievance_theme != "NA") %>%
  ggplot(aes(x=Grievance_theme, fill=Grievance_theme)) + geom_bar() +
  labs(x="Grievance theme", y="Count", fill="Grievance theme", title="Count of Grievance themes")

# 3.2 Sub-Theme
# Freq table
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
# Bar graph
table_analysis %>% # All sub-themes
  melt(measure.vars = string_subtheme, variable.name='Sub_theme') %>% 
  filter(value=="1") %>% 
  ggplot(aes(y = fct_rev(Sub_theme))) + geom_bar() + 
  labs(y = 'Sub theme', title="Count of sub themes")
table_analysis %>% # Environmental themed
  melt(measure.vars = string_subtheme, variable.name='Sub_theme') %>% 
  filter(value=="1", Grievance_theme == "Environmental") %>%
  ggplot(aes(y = fct_rev(Sub_theme))) + geom_bar() + 
  labs(y = 'Sub theme', x="Count", title="Sub themes (Environmental)")
table_analysis %>% # Social themed
  melt(measure.vars = string_subtheme, variable.name='Sub_theme') %>% 
  filter(value=="1", Grievance_theme == "Social") %>%
  ggplot(aes(y = fct_rev(Sub_theme))) + geom_bar() + 
  labs(y = 'Sub theme', fill="Sub theme", title="Sub themes (Social)")
table_analysis %>% # Both themed
  melt(measure.vars = string_subtheme, variable.name='Sub_theme') %>% 
  filter(value=="1", Grievance_theme == "Both") %>%
  ggplot(aes(y = fct_rev(Sub_theme))) + geom_bar() + labs(y = 'Sub theme', title="Sub themes (Both)")

# 3.3 Issues
# Freq table
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
# Bar graph
table_analysis %>% # All issues
  melt(measure.vars = string_issue, variable.name="Issue") %>% 
  filter(value == "1") %>% 
  ggplot(aes(y=fct_rev(Issue))) + geom_bar() +
  labs(x = "Count", y="Issue", title="Count of Issues")
table_analysis %>% # Environmental themed
  melt(measure.vars = string_issue, variable.name="Issue") %>%
  filter(value == "1", Grievance_theme=="Environmental") %>%
  ggplot(aes(y=fct_rev(Issue))) + geom_bar() +
  labs(x = "Count", y="Issue", title="Issues (Environmental")
table_analysis %>% # Social themed
  melt(measure.vars = string_issue, variable.name="Issue") %>% 
  filter(value == "1", Grievance_theme=="Social") %>%
  ggplot(aes(y=fct_rev(Issue))) + geom_bar() +
  labs(x = "Count", y="Issue", title="Issues (Social")
table_analysis %>% # Both themed
  melt(measure.vars = string_issue, variable.name="Issue") %>% 
  filter(value == "1", Grievance_theme=="Both") %>%
  ggplot(aes(y=fct_rev(Issue))) + geom_bar() +
  labs(x = "Count", y="Issue", title="Issues (Both")

# 4.0 Sourcing relationship
# Freq table
table_analysis %>% 
  filter(GC_sourcing != "NA") %>% 
  group_by(GC_sourcing) %>% summarise(Count = n()) %>%
  mutate(Percent = round((Count/sum(Count)*100), digits = 1))  %>%
  write.csv(quote=F, row.names=F)
# Bar graph
table_analysis %>% 
  filter(!GC_sourcing %in% c('Unable to determine', 'Not in supply chain', 'NA')) %>%
  ggplot(aes(x = GC_sourcing, fill=GC_sourcing)) + geom_bar() +
  labs(x="Sourcing relationship") + theme(legend.position = 0)

# 4.1 Sourcing by Engagement channel
# Bar graph of counts
table_analysis %>%
  melt(measure.vars= c('Channel_direct', 'Channel_indirect', 'Channel_strategic_alliance', 'Channel_third_party'),
                        variable.name = "channel") %>%
  filter(!GC_sourcing %in% c('Unable to determine', 'Not in supply chain', 'NA'), value== "1") %>%
  ggplot(aes(x= GC_sourcing, fill=channel)) + geom_bar() + 
  labs(x="Sourcing relationship", fill="Engagement channel") + 
  scale_fill_discrete(labels = c("Direct", "Indirect", "Strategic alliance", "Third party"))
# Bar graph of proportions
table_analysis %>% 
  melt(measure.vars= c('Channel_direct', 'Channel_indirect', 'Channel_strategic_alliance', 'Channel_third_party'),
                        variable.name = "channel") %>%
  filter(!GC_sourcing %in% c('Unable to determine', 'Not in supply chain', 'NA'), value== "1") %>%
  ggplot(aes(x= GC_sourcing, fill=channel)) + geom_bar(position="fill") + 
  labs(x="Sourcing relationship", fill="Engagement channel") +
  scale_fill_discrete(labels = c("Direct", "Indirect", "Strategic alliance", "Third party")) +
  scale_y_continuous(labels = scales::percent) + theme(axis.title.y = element_blank())
# Freq table
table_analysis %>% 
  melt(measure.vars= c('Channel_direct', 'Channel_indirect', 'Channel_strategic_alliance', 'Channel_third_party'),
                        variable.name = "channel") %>%
  filter(!GC_sourcing %in% c('Unable to determine', 'Not in supply chain', 'NA'), value== "1") %>%
  group_by(GC_sourcing, channel) %>% summarise(Count = n()) %>%
  mutate(Percent = round((Count/sum(Count)*100), digits = 1))  %>% 
  write.csv(quote=F, row.names = F)

# 5.0 Focal company
# 5.1 Focal company process
# Freq table
table_analysis %>% 
  melt(measure.vars=string_Focal_process, variable.name="Focal_process") %>% filter(value=="1") %>% 
  group_by(Focal_process) %>% summarise(Count = n()) %>%
  mutate(Percent = round((Count/sum(Count)*100), digits = 1)) %>% 
  write.csv(quote=F, row.names=F)
# Bar graph
table_analysis %>% 
  melt(measure.vars=string_Focal_process, variable.name="Focal_process") %>% filter(value=="1") %>% 
  ggplot(aes(y = fct_rev(Focal_process))) + geom_bar() + 
  labs(y="Focal process")
# 5.2 Focal company resolution action
# Freq table
table_analysis %>% 
  filter(Focal_resolution != "NA") %>%
  group_by(Focal_resolution) %>% summarise(Count = n()) %>%
  mutate(Percent = round((Count/sum(Count)*100), digits = 1)) %>%
  arrange(Count) %>%
  write.csv(quote=F, row.names=F)
# Bar graph 
table_analysis %>% filter(Focal_resolution != "NA") %>%
  ggplot(aes(x='', fill= fct_rev(fct_infreq(Focal_resolution)))) + geom_bar(position="fill") +
  labs(x='Focal company resolution action',y='', fill="Resolution action") + 
  scale_y_continuous(labels=scales::percent) +
  scale_fill_discrete(labels=c("Coop-SCM, Coer-SCM: 1%", "Improve company practice: 2%", "Remedial: 3%",
                               "Coop-SCM: 11%", "Re-entry: 20%", "Coer-SCM: 63%"))
# 5.3 Focal company supplier engagement
# Freq table
table_analysis %>% 
  filter(Engagement_nature != "NA") %>% 
  group_by(Engagement_nature) %>% summarise(Count = n()) %>%
  mutate(Percent = round((Count/sum(Count)*100), digits = 1)) %>% 
  write.csv(quote=F, row.names=F)
# Bar graph
table_analysis %>% filter(Engagement_nature != "NA") %>%
  ggplot(aes(x='', fill=Engagement_nature)) + geom_bar(position="fill") + 
  labs(x="Engagement nature", y="", fill="Engagement nature") + 
  scale_y_continuous(labels=scales::percent) +
  scale_fill_discrete(labels=c("Both: 31%", "Coercive: 17%", "Cooperative: 52%"))

# 6.0 Overall grievance outcomes
# Freq table
table_analysis %>% 
  filter(!is.na(Grievance_outcome)) %>%
  group_by(Grievance_outcome) %>% summarise(Count = n()) %>%
  mutate(Percent = round((Count/sum(Count)*100), digits = 1)) %>% 
  write.csv(quote=F, row.names=F)
# Bar graph
table_analysis %>% filter(!is.na(Grievance_outcome))  %>%
  ggplot(aes(x='', fill= fct_rev(fct_infreq(Grievance_outcome)))) + geom_bar(position="fill") +
  labs(x='Overall grievance outcome', y='', fill='') +
  scale_y_continuous(labels=scales::percent) + 
  scale_fill_discrete(labels=c('Direct action: 2%', 'Supplier support: 4%', 'Nullified: 5%', 'Policy compliance: 5%',
                            'Re-entry: 7%', 'Multiplier: 9%', 'Suspend: 25%', 'Monitor: 43%'))

# 7.0 Grievance characteristics
# Freq table
table_analysis %>% 
  filter(Company != "NA") %>% 
  melt(measure.vars=c("GC_clarity", "GC_status", "GC_number", "GC_sourcing"),
       variable.name = "Characteristic", value.name = "Measure") %>%
  group_by(Characteristic) %>% count(Measure) %>% 
  mutate(Percent = round((100*n/sum(n)), digits = 1)) %>% 
  write.csv(quote=F, row.names=F)
