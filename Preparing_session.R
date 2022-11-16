## Code to create dataframe table from csv file that will be used for analysis. Also has data objects to help
## reshape data from wide to long format
# library path, if needed
.libPaths("C:\\Users\\ASEuser\\OneDrive - Nanyang Technological University\\NTU RA\\Grievance Database-Project docs\\Analysis\\R\\win-library")
# Packages used 
library(readxl)   # read excel
library(tidyverse)# core functions, cite
library(rlang)    # support tidyverse features
library(ggpubr)   # arrange multiple plots in single figure
library(summarytools) # create data summaries like freq table, cite
library(lubridate)# work with dates and time, calculate interval
library(reshape2) # change data to long format
library(janitor) # help change date to numeric
library(ggmosaic) # make mosaic plots in ggplot, without stats, cite
library(sjPlot) # make nice contingency tables with stats, cite
library(RColorBrewer) # change color palette, cite
library(rcompanion) # conduct cramer V test
library(vcd) # make mosaic plots with stats
library(nnet) # conduct multinomial regression
library(readr)
library(sjstats) 
# Import excel database
mydata <- read_excel("C:\\Users\\ASEuser\\OneDrive - Nanyang Technological University\\NTU RA\\Grievance Database-Project docs\\GrievanceProcedure_v2_20221111.xlsx")
# Create table for analysis. This dataset is used for all subsequent analysis. 
table_analysis <- mydata %>%
  transmute(ID = UID, Company = COMPANY, Status = STATUS, Country = COUNTRY, Grievance_raiser = `GRIEVANCE RAISER`, GR_type = `GRIEVANCE RAISER TYPE`,
            Acc_foc_rs = `ACCUSED TO FOCAL RELATIONSHIP`, Int_foc_rs = `INTERMEDIATE COMPANY RELATIONSHIP`,
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
            FR_Remedial = `FR-REMEDIAL`, FR_Improve_prac = `FR-IMPROVE PRACTICE`, FR_Coop_SCM = `FR-COOP SCM`, 
            FR_Coer_SCM = `FR-COER SCM`, FR_Re_entry = `FR-RENTRY`,
            SP_info_facil = `SP-INFORMATION FACILITATION`, SP_negotiation = `SP-NEGOTIATION`, SP_mediation = `SP-MEDIATION`,
            SP_investigation = `SP-INVESTIGATION`, SP_adjudication = `SP-ADJUDICATION`, SP_socialisation = `SP-SOCIALISATION`,
            SP_support = `SP-SUPPORT`, # Supplier process end
            Supplier_resolution_type = `SUPPLIER RESOLUTION TYPE`, 
            SR_Remedial = `SR-REMEDIAL`, SR_Improve_prac = `SR-IMPROVE PRACTICE`, SR_Policy = `SR-POLICY`, SR_SSCM = `SR-SSCM`,
            GC_clarity = `CLARITY OF ALLEGATION`, GC_status = `STATUS OF GRIEVANCE RAISER`, GC_number = `NUMBER OF AGGRIEVED PARTIES`,
            GC_sourcing = `SOURCING RELATIONSHIP`, Complexity = as.numeric(`COMPLEXITY SCORE`), Grievance_outcome = `GRIEVANCE OUTCOME`) %>%
  mutate(Grievance_outcome = factor(Grievance_outcome, levels=c("Nullified", "Monitor", "Direct action", "Policy compliance",
                                                                "Supplier support", "Multiplier", "Suspend", "Re-entry")))
# Format dates as numeric to calculate duration
Date_lodged <- excel_numeric_to_date(as.numeric(mydata$`DATE LODGED`))
Date_closed <- excel_numeric_to_date(as.numeric(mydata$`DATE CLOSED`))
Date_last_purchase <- excel_numeric_to_date(as.numeric(mydata$`DATE LAST PURCHASE`))
Date_reentry <- excel_numeric_to_date(as.numeric(mydata$`DATE RE-ENTRY`))
Date_first_engagement <- excel_numeric_to_date(as.numeric(mydata$`DATE FIRST ENGAGEMENT`))
# Add duration columns to "table_analysis" dataset
table_analysis <- table_analysis %>% mutate(Duration_lodged_closed = time_length(interval(Date_lodged, Date_closed), unit="months")) %>%
  relocate(Duration_lodged_closed, .after = Country) %>%
  mutate(Duration_lodged_closed = replace(Duration_lodged_closed, which(Duration_lodged_closed < 0), NA))

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
string_Focal_resolution <- c('FR_Remedial', 'FR_Improve_prac', 'FR_Coop_SCM', 'FR_Coer_SCM', 'FR_Re_entry')
string_Supplier_process <- c('SP_info_facil', 'SP_negotiation', 'SP_mediation', 'SP_investigation',
                             'SP_adjudication', 'SP_socialisation', 'SP_support')
string_Supplier_resolution <- c('SR_Remedial', 'SR_Improve_prac', 'SR_Policy', 'SR_SSCM')