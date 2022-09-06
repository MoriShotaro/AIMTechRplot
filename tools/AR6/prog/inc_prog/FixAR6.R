# Input original data -----------------------------------------------------

dfAR6_origin <- fread(paste0(tdir,"/AR6/output/AR6_Scenario_Database.csv"),stringsAsFactors=FALSE,header=TRUE) %>% 
  pivot_longer(cols=as.character(seq(2010,2100,5)),names_to="Year",values_to="Value") %>%
  mutate(Year=as.numeric(Year)) %>% 
  rename("Variable"=GAMS.code) %>% 
  pivot_wider(names_from=Year,values_from=Value,names_prefix="Y")


# Detect start and end year of each variable ------------------------------

dfAR6_yst <- dfAR6_origin %>%
  pivot_longer(cols=c(Y2010:Y2100),names_to="Year",values_to="Value",names_prefix="Y") %>% 
  drop_na(Value) %>%
  select(Name,Variable,Year,Value) %>%
  group_by(Name,Variable) %>%
  summarise(Ystart=min(Year)) %>% 
  full_join(dfAR6_origin) %>% 
  select(Name,Variable,Ystart)

dfAR6_yed <- dfAR6_origin %>%
  pivot_longer(cols=c(Y2010:Y2100),names_to="Year",values_to="Value",names_prefix="Y") %>% 
  drop_na(Value) %>%
  select(Name,Variable,Year,Value) %>%
  group_by(Name,Variable) %>%
  summarise(Yend=max(Year)) %>% 
  full_join(dfAR6_origin) %>% 
  select(Name,Variable,Yend)

dfAR6 <- dfAR6_origin %>%
  left_join(dfAR6_yst) %>%
  left_join(dfAR6_yed)

# Detect Y5 variable of each model ----------------------------------------

dfAR6_Y <- dfAR6 %>%
  pivot_longer(cols=c(Y2010:Y2100),names_to="Year",values_to="Value",names_prefix="Y") %>%
  mutate(Year=as.numeric(Year)) %>%
  filter(Year>=Ystart&Year<=Yend) %>% 
  select(Name,Variable,Year,Value,Ystart,Yend)

dfAR6_Y5var <- dfAR6_Y %>%
  group_by(Name,Variable) %>%
  summarize(NAcount_Y5=sum(is.na(Value))) %>% 
  mutate(Variable_Y5=if_else(NAcount_Y5==0,TRUE,FALSE)) %>%
  select(-NAcount_Y5)


# Detect Y10 variable of each model ---------------------------------------

dfAR6_Y10var <- dfAR6_Y %>%
  left_join(dfAR6_Y5var) %>% 
  filter(Year%in%seq(2010,2100,10)) %>%
  group_by(Name,Variable) %>%
  summarize(NAcount_Y10=sum(is.na(Value))) %>% 
  mutate(Variable_Y10=if_else(NAcount_Y10==0,TRUE,FALSE)) %>%
  select(-NAcount_Y10)



# Merge and output --------------------------------------------------------

df <- dfAR6 %>% 
  left_join(dfAR6_Y5var) %>% 
  left_join(dfAR6_Y10var)
write_csv(df,paste0(tdir,"/AR6/output/AR6_Scenario_Database_v2.csv"))

Var_Enduse <- "(Prm_Ene)|(Sec_Ene)|(Fin_Ene)|(Emi_CO2)|(Car_Cap)|(Car_Seq)|(Prc_Car)|(Pol_Cos_Add_Tot_Ene_Sys_Cos)|(Inv_Add)"
df_Enduse <- df %>%
  filter(str_detect(Variable,Var_Enduse)) %>% 
  select(-Template.code)
write_csv(df_Enduse,paste0(tdir,"/AR6/output/AR6_Scenario_Database_Enduse.csv"))
