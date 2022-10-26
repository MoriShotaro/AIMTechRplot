ddir2 <- paste(cdir,"data",date,project,"main_wo",sep="/")

df_all <- rgdx.param(paste(ddir,"merged_output.gdx",sep="/"),"iamc_gdx") %>% 
  rename(Scenario='Sc',Region='Sr',Variable='Sv',Year='Y5',Value='iamc_gdx') %>%
  pivot_wider(names_from=Year,values_from=Value,values_fill=0) %>% 
  pivot_longer(cols=-c(Scenario,Region,Variable),names_to="Year",values_to="Value") %>%
  mutate(Year=as.numeric(as.character(Year)),Region=as.character(Region),Scenario=as.character(Scenario)) %>% 
  filter(Region%in%reg_in)

df_all2 <- rgdx.param(paste(ddir2,"merged_output.gdx",sep="/"),"iamc_gdx") %>% 
  rename(Scenario='Sc',Region='Sr',Variable='Sv',Year='Y5',Value='iamc_gdx') %>%
  pivot_wider(names_from=Year,values_from=Value,values_fill=0) %>% 
  pivot_longer(cols=-c(Scenario,Region,Variable),names_to="Year",values_to="Value") %>%
  mutate(Year=as.numeric(as.character(Year)),Region=as.character(Region),Scenario=as.character(Scenario)) %>% 
  filter(Region%in%reg_in) %>% 
  mutate(Scenario=recode(Scenario,"Baseline"="Baseline_w/o_CTLGTL","SR15_LOS"="SR15_LOS_w/o_CTLGTL"))

df_all <- df_all %>% filter(Scenario!="SR15_HOS") %>% 
  bind_rows(df_all2)

scen_mat <- read_csv(paste(ddir,"scenario_table.csv",sep="/")) %>% 
  filter(Scenario!="SR15_HOS")

scen_mat2 <- scen_mat %>% 
  filter(Scenario!="SR15_HOS") %>% 
  mutate(Scenario=recode(Scenario,"Baseline"="Baseline_w/o_CTLGTL","SR15_LOS"="SR15_LOS_w/o_CTLGTL")) %>%
  mutate(scen_techpol=as.character(scen_techpol)) %>% 
  replace_na(list(scen_techpol="w/o CTLGTL"))

scen_mat <- scen_mat %>%
  mutate(scen_techpol=as.character(scen_techpol)) %>% 
  replace_na(list(scen_techpol="w/ CTLGTL")) %>% 
  bind_rows(scen_mat2)
