# Input data --------------------------------------------------------------

df_IAMCvchg <- read.table(paste0(tdir,"/AR6/define/IAMCVariableChange/IAMCTemplate.txt"),sep="\t",header=TRUE) %>% 
  select(GAMS.code,Template.code)
df_category <- read.table(paste0(tdir,"/AR6/define/AR6category.txt"),sep="\t",header=TRUE)

df <- fread(paste0(tdir,"/AR6/data/AR6_Scenario_Database_origin.csv"),stringsAsFactors=FALSE,header=TRUE)

dfAR6 <- df %>%
  select(-Region,-Unit) %>%
  rename("Template.code"=Variable) %>% 
  inner_join(df_IAMCvchg) %>%
  filter(!is.na(GAMS.code)) %>%
  unite(col=Name,Model,Scenario,sep="|",remove=FALSE) %>%
  inner_join(df_category) %>%
  select(Name,Model,Scenario,Template.code,GAMS.code,category,everything()) %>% 
  filter(!is.na(category)) %>%
  pivot_longer(cols=-c(Name,Model,Scenario,Template.code,GAMS.code,category),names_to="Year",values_to="Value") %>% 
  filter(Year%in%year_set) %>% 
  pivot_wider(names_from=Year,values_from=Value)

write_csv(dfAR6,paste0(tdir,"/AR6/output/AR6_Scenario_Database.csv"))
