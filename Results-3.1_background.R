## Results 3.1: Background of grievance cases
## Note: I chose to convert tables to csv so that further formatting and changes can be made on excel as I am still unfamiliar with 
## how to achieve the desired output entirely in code. 

# Count of cases, by focal company
table_analysis %>% filter(Company != "NA") %>%
  freq(Company) %>% write.csv(quote = F)
# Count of cases, by status
table_analysis %>% filter(Status != "NA") %>%
  freq(Status) %>% write.csv(quote = F)
# Count of cases, by Country
table_analysis %>% filter(Country != "NA") %>%
  freq(Country, order="freq") %>% write.csv(quote=F)
## Note: number of cases as reported in grievance website was calculated by manually looking at database UID numbers.
