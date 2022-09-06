df_all <- rgdx.param(paste(ddir,"merged_output.gdx",sep="/"),"iamc_gdx") %>% 
  rename(Scenario='Sc',Region='Sr',Variable='Sv',Year='Y5',Value='iamc_gdx') %>%
  pivot_wider(names_from=Year,values_from=Value,values_fill=0) %>% 
  pivot_longer(cols=-c(Scenario,Region,Variable),names_to="Year",values_to="Value") %>%
  mutate(Year=as.numeric(as.character(Year)),Region=as.character(Region),Scenario=as.character(Scenario)) %>% 
  filter(Region%in%reg_in)

date2 <- "2208041109"
project <- "model_expansion"
ddir2 <- paste(cdir,"data",project,date2,"main",sep="/")

df_all2 <- rgdx.param(paste(ddir2,"merged_output.gdx",sep="/"),"iamc_gdx") %>% 
  rename(Scenario='Sc',Region='Sr',Variable='Sv',Year='Y5',Value='iamc_gdx') %>%
  pivot_wider(names_from=Year,values_from=Value,values_fill=0) %>% 
  pivot_longer(cols=-c(Scenario,Region,Variable),names_to="Year",values_to="Value") %>%
  mutate(Year=as.numeric(as.character(Year)),Region=as.character(Region),Scenario=as.character(Scenario)) %>% 
  filter(Region%in%reg_in) %>% 
  filter(Scenario=="Baseline") %>% 
  mutate(Scenario=recode(Scenario,"Baseline"="Baseline_w/o_CTLGTL"))

df_all <- bind_rows(df_all,df_all2)

scen_mat <- read_csv(paste(ddir,"scenario_table.csv",sep="/"))

scen_mat2 <- scen_mat %>% 
  mutate(Scenario=recode(Scenario,"Baseline"="Baseline_w/o_CTLGTL")) %>%
  mutate(scen_techpol=as.character(scen_techpol)) %>% 
  replace_na(list(scen_techpol="w/o CTLGTL"))

scen_mat <- scen_mat %>%
  mutate(scen_techpol=as.character(scen_techpol)) %>% 
  replace_na(list(scen_techpol="w/ CTLGTL")) %>% 
  bind_rows(scen_mat2)
