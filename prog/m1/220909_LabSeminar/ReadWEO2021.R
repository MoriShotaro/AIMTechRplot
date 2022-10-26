ddir3 <- paste(cdir,"data",date,project,sep="/")

df <- read_csv(paste0(ddir3,"/WEO2021_Free_Data_Supply_Refining.csv")) %>% 
  filter(SCENARIO=="Stated Policies Scenario") %>% 
  filter(PRODUCT%in%c("Oil","CTL, GTL and additives")) %>%
  filter(FLOW=="Demand") %>% 
  filter(YEAR==2020|YEAR==2019) %>%
  mutate(Value=VALUE*10^6*365*159*0.925*41.868/1000/10^9)