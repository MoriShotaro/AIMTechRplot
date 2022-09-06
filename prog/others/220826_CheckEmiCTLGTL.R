check <- df_all %>% 
  filter(Variable%in%c("Prm_Ene_Coa","Prm_Ene_Oil","Prm_Ene_Gas")|Variable=="Emi_CO2_Ene_and_Ind_Pro") %>% 
  filter(Scenario=="Baseline")

check2 <- data.frame(Variable=c("Prm_Ene_Coa","Prm_Ene_Oil","Prm_Ene_Gas","Emi_CO2_Ene_and_Ind_Pro"),emf=c(94.6,77.4,56.1,1),flag=c("Fos","Fos","Fos","Emi"))

check3 <- full_join(check,check2) %>% 
  mutate(Check=Value*emf) %>% 
  group_by(flag,Year) %>% 
  summarise(Check=sum(Check)) %>% 
  pivot_wider(names_from=flag,values_from=Check) %>% 
  mutate(Diff=(Emi-Fos)/Fos) %>% 
  mutate(Scenario="with")


date2 <- "2208041109"
project <- "model_expansion"
ddir2 <- paste(cdir,"data",project,date2,"main",sep="/")

df_all2 <- rgdx.param(paste(ddir2,"merged_output.gdx",sep="/"),"iamc_gdx") %>% 
  rename(Scenario='Sc',Region='Sr',Variable='Sv',Year='Y5',Value='iamc_gdx') %>%
  pivot_wider(names_from=Year,values_from=Value,values_fill=0) %>% 
  pivot_longer(cols=-c(Scenario,Region,Variable),names_to="Year",values_to="Value") %>%
  mutate(Year=as.numeric(as.character(Year)),Region=as.character(Region),Scenario=as.character(Scenario)) %>% 
  filter(Region%in%reg_in) %>% 
  filter(Scenario=="Baseline")

check4 <- df_all2 %>% 
  filter(Variable%in%c("Prm_Ene_Coa","Prm_Ene_Oil","Prm_Ene_Gas")|Variable=="Emi_CO2_Ene_and_Ind_Pro") %>% 
  filter(Scenario=="Baseline")

check5 <- data.frame(Variable=c("Prm_Ene_Coa","Prm_Ene_Oil","Prm_Ene_Gas","Emi_CO2_Ene_and_Ind_Pro"),emf=c(94.6,77.4,56.1,1),flag=c("Fos","Fos","Fos","Emi"))

check6 <- full_join(check4,check5) %>% 
  mutate(Check=Value*emf) %>% 
  group_by(flag,Year) %>% 
  summarise(Check=sum(Check)) %>% 
  pivot_wider(names_from=flag,values_from=Check) %>% 
  mutate(Diff=abs(Emi-Fos)/Fos) %>% 
  mutate(Scenario="without")

df <- bind_rows(check3,check6) %>%
  select(Scenario,Year,Fos) %>% 
  pivot_wider(names_from=Scenario,values_from=Fos) %>% 
  mutate(Diff=abs(with-without)/without)

df2 <- bind_rows(check3,check6) %>%
  select(Scenario,Year,Emi) %>% 
  pivot_wider(names_from=Scenario,values_from=Emi) %>% 
  mutate(Diff=abs(with-without)/without)

g <- df %>%
  pivot_longer(cols=c(with,without),names_to="Variable",values_to="Value") %>% 
  ggplot()+
  geom_line(aes(x=Year,y=Value,group=Variable,color=Variable))
plot(g)

g2 <- df2 %>%
  pivot_longer(cols=c(with,without),names_to="Variable",values_to="Value") %>% 
  ggplot()+
  geom_line(aes(x=Year,y=Value,group=Variable,color=Variable))
plot(g2)

hoge <- df_all2 %>% 
  filter(str_detect(Variable,"Emi_CO2"))

g3 <- bind_rows(check3,check6) %>% 
  select(-Diff) %>% 
  pivot_longer(cols=c(Emi,Fos),names_to="Variable",values_to="Value") %>% 
  ggplot() +
  geom_line(aes(x=Year,y=Value,group=Variable,color=Variable)) +
  facet_wrap(vars(Scenario))
plot(g3)
  
