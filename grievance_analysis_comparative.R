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
############################
## Create table for analysis
# Import database excel
mydata <- read_excel("C:\\Users\\ASEuser\\OneDrive - Nanyang Technological University\\NTU RA\\Grievance Database-Project docs\\GrievanceProcedure_v2_20220906.xlsx")
# Create base table
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
############################
## Duration as Outcome variable
# 1.1 Complexity vs Duration
# Stats table
table_analysis %>% 
  filter(Status=="Closed") %>%
  group_by(Complexity) %>% 
  summarise(Count = n(), Min = round((min(Duration_lodged_closed, na.rm = T)), digits=1),
            Quant1 = round((quantile(Duration_lodged_closed, probs = 0.25, na.rm = T)), digits=1),
            Med = round((median(Duration_lodged_closed, na.rm = T)), digits=1),
            Mean = round((mean(Duration_lodged_closed, na.rm = T)), digits=1),
            Quant3 = round((quantile(Duration_lodged_closed, probs = 0.75, na.rm = T)), digits=1),
            Max = round((max(Duration_lodged_closed, na.rm = T)), digits=1)) %>%
  write.csv(quote=F, row.names=F)
# Jitter and boxplot
table_analysis %>% 
  filter(Status=="Closed") %>%
  ggplot(aes(x = as.factor(Complexity), y = Duration_lodged_closed)) + 
  geom_boxplot(width = 0.1, colour = '#FF0000', outlier.shape=NA) +
  geom_jitter(position = position_jitter(0.2, 0.2), alpha = 0.5) +
  labs(x = "Complexity", y = "Duration (months)")

# 1.2 Clarity vs Duration
# Stats table
table_analysis %>% filter(Status=="Closed", GC_clarity != "NA") %>%
  group_by(GC_clarity) %>% 
  summarise(Count = n(), Min = round((min(Duration_lodged_closed, na.rm = T)), digits=1),
            Quant1 = round((quantile(Duration_lodged_closed, probs = 0.25, na.rm = T)), digits=1),
            Med = round((median(Duration_lodged_closed, na.rm = T)), digits=1),
            Mean = round((mean(Duration_lodged_closed, na.rm = T)), digits=1),
            Quant3 = round((quantile(Duration_lodged_closed, probs = 0.75, na.rm = T)), digits=1),
            Max = round((max(Duration_lodged_closed, na.rm = T)), digits=1)) %>%
  write.csv(quote=F, row.names=F)
# Jitter and boxplot
table_analysis %>% filter(Status=="Closed", GC_clarity != "NA") %>%
  ggplot(aes(x= GC_clarity, y = Duration_lodged_closed)) + 
  geom_boxplot(width=0.2, colour='#FF0000', outlier.shape=NA) + 
  geom_jitter(position = position_jitter(0.2, 0.2), alpha=0.5) +
  labs(x = "Grievance clarity", y = "Duration of resolution")

# 1.3 Grievance raiser status vs Duration
# Stats table
table_analysis %>% filter(Status=="Closed", GC_status != "NA") %>%
  group_by(GC_status) %>% 
  summarise(Count = n(), Min = round((min(Duration_lodged_closed, na.rm = T)), digits=1),
            Quant1 = round((quantile(Duration_lodged_closed, probs = 0.25, na.rm = T)), digits=1),
            Med = round((median(Duration_lodged_closed, na.rm = T)), digits=1),
            Mean = round((mean(Duration_lodged_closed, na.rm = T)), digits=1),
            Quant3 = round((quantile(Duration_lodged_closed, probs = 0.75, na.rm = T)), digits=1),
            Max = round((max(Duration_lodged_closed, na.rm = T)), digits=1)) %>%
  write.csv(quote=F, row.names=F)
# Jitter and boxplot
table_analysis %>% 
  filter(Status=="Closed", GC_status != "NA") %>%
  ggplot(aes(x = GC_status, y = Duration_lodged_closed)) + 
  geom_boxplot(width = 0.2, colour='#FF0000', outlier.shape=NA) +
  geom_jitter(position = position_jitter(0.2, 0.2), alpha=0.5) +
  labs(x="Status of grievance raiser", y="Duration of resolution")

# 1.4 Number of aggrieved parties vs Duration
# Stats table
table_analysis %>% filter(Status=="Closed", !GC_number %in% c("4", "NA")) %>%
  group_by(GC_number) %>%  
  summarise(Count = n(), Min = round((min(Duration_lodged_closed, na.rm = T)), digits=1),
            Quant1 = round((quantile(Duration_lodged_closed, probs = 0.25, na.rm = T)), digits=1),
            Med = round((median(Duration_lodged_closed, na.rm = T)), digits=1),
            Mean = round((mean(Duration_lodged_closed, na.rm = T)), digits=1),
            Quant3 = round((quantile(Duration_lodged_closed, probs = 0.75, na.rm = T)), digits=1),
            Max = round((max(Duration_lodged_closed, na.rm = T)), digits=1)) %>%
  write.csv(quote=F, row.names=F)
# jitter and boxplot
table_analysis %>% 
  filter(Status=="Closed", !GC_number %in% c("4", "NA")) %>%
  ggplot(aes(x = as.factor(GC_number), y = Duration_lodged_closed)) + 
  geom_boxplot(width = 0.2, colour='#FF0000', outlier.shape=NA) +
  geom_jitter(position = position_jitter(0.2, 0.2), alpha=0.5) +
  labs(x="Number of aggrieved parties", y="Duration of resolution")

# 1.5 Sourcing relationship vs Duration
# Stats table
table_analysis %>% 
  filter(Status=="Closed", !GC_sourcing %in% c("NA", "Not in supply chain")) %>%
  group_by(GC_sourcing) %>%   
  summarise(Count = n(), Min = round((min(Duration_lodged_closed, na.rm = T)), digits=1),
            Quant1 = round((quantile(Duration_lodged_closed, probs = 0.25, na.rm = T)), digits=1),
            Med = round((median(Duration_lodged_closed, na.rm = T)), digits=1),
            Mean = round((mean(Duration_lodged_closed, na.rm = T)), digits=1),
            Quant3 = round((quantile(Duration_lodged_closed, probs = 0.75, na.rm = T)), digits=1),
            Max = round((max(Duration_lodged_closed, na.rm = T)), digits=1)) %>%
  write.csv(quote=F, row.names=F)
# Jitter and boxplot
table_analysis %>% 
  mutate(GC_sourcing = factor(GC_sourcing, levels = c("Vertical", "Direct", "Indirect",
                                                      "Not in supply chain", "Unable to determine", "NA"))) %>%
  filter(Status=="Closed", !GC_sourcing %in% c("NA", "Not in supply chain")) %>%
  ggplot(aes(x = GC_sourcing, y = Duration_lodged_closed)) + 
  geom_boxplot(width = 0.2, colour='#FF0000', outlier.shape=NA) +
  geom_jitter(position = position_jitter(0.2, 0.2), alpha=0.5) +
  labs(x="Sourcing relationship", y="Duration of resolution")

# 1.6 Theme vs Duration
# Stats table
table_analysis %>% filter(Status=="Closed", Grievance_theme != "NA") %>%
  group_by(Grievance_theme) %>%   
  summarise(Count = n(), Min = round((min(Duration_lodged_closed, na.rm = T)), digits=1),
            Quant1 = round((quantile(Duration_lodged_closed, probs = 0.25, na.rm = T)), digits=1),
            Med = round((median(Duration_lodged_closed, na.rm = T)), digits=1),
            Mean = round((mean(Duration_lodged_closed, na.rm = T)), digits=1),
            Quant3 = round((quantile(Duration_lodged_closed, probs = 0.75, na.rm = T)), digits=1),
            Max = round((max(Duration_lodged_closed, na.rm = T)), digits=1)) %>%
  write.csv(quote=F, row.names=F)
# Jitter and boxplot
table_analysis %>% 
  filter(Status=="Closed", Grievance_theme != "NA") %>%
  ggplot(aes(x = Grievance_theme, y = Duration_lodged_closed)) + 
  geom_boxplot(width = 0.2, colour='#FF0000', outlier.shape=NA) +
  geom_jitter(position = position_jitter(0.2, 0.2), alpha=0.5) +
  labs(x="Grievance theme", y="Duration of resolution")

## Grievance outcome as outcome variable
# 2.1 Complexity vs Outcome
# Freq table
table_analysis %>% 
  filter(Grievance_outcome != "NA") %>%
  select(Complexity, Grievance_outcome) %>%
  sjtab(fun = "xtab", var.labels=c("Complexity", "Grievance_outcome"), 
        show.exp = F, show.row.prc = T, show.col.prc = T, show.legend = T, show.summary = F)
# Mosaic plot
table_analysis %>% 
  filter(Grievance_outcome != "NA") %>%
  ggplot() + 
  geom_mosaic(aes(x= product(Grievance_outcome, Complexity), fill= Grievance_outcome), offset = 0.03) +
  labs(x="Complexity", y="Grievance outcome", fill="Grievance outcome")

# 2.2 Sourcing relationship vs Outcome
# Stats table
table_analysis %>%
  filter(Status=="Closed",
         !GC_sourcing %in% c("NA", "Not in supply chain")) %>%
  select(GC_sourcing, Grievance_outcome) %>%
  sjtab(fun = "xtab", var.labels=c("Sourcing relationship", "Grievance outcome"), 
        show.exp = F, show.row.prc = T, show.col.prc = T, show.legend = T, show.summary = F)
# Mosaic plot
table_analysis %>% 
  filter(Status=="Closed", 
         !GC_sourcing %in% c("NA", "Not in supply chain"),
         Grievance_outcome != "NA") %>%
  ggplot() + 
  geom_mosaic(aes(x= product(Grievance_outcome, GC_sourcing), fill= Grievance_outcome), offset=0.02) +
  labs(x="Sourcing relationship", y="Grievance outcome", fill="Grievance outcome")

# 2.3 Theme vs Outcome
# Stats table
table_analysis %>% 
  filter(Status=="Closed") %>%
  select(Grievance_theme, Grievance_outcome) %>%
  sjtab(fun = "xtab", var.labels=c("Grievance theme", "Grievance outcome"), 
        show.exp = F, show.row.prc = T, show.col.prc = T, show.legend = T, show.summary = F)
# Mosaic plot
table_analysis %>% 
  filter(Status=="Closed", Grievance_outcome != "NA") %>%
  ggplot() + 
  geom_mosaic(aes(x= product(Grievance_outcome, Grievance_theme), fill= Grievance_outcome), offset=0.02) +
  labs(x="Grievance theme", y="Grievance outcome", fill="Grievance outcome")
