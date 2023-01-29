# Option ------------------------------------------------------------------

general <- FALSE
index <- FALSE
bind_baseline <- TRUE

# library -----------------------------------------------------------------

library(tidyverse)
library(gdxrrw)
library(stringr)
library(patchwork)
library(readr)
library(scales)
library(data.table)
library(purrr)
library(readxl)
library(countrycode)

# Setting -----------------------------------------------------------------

# General information
date <- "m1"
project <- "221003_Industry"
cdir <- getwd()
odir <- paste(cdir,"output",date,project,sep="/")
ddir <- paste(cdir,"data",date,project,sep="/")
pdir <- paste(cdir,"prog",sep="/")
pdir2 <- paste(pdir,date,project,sep="/")
mdir <- paste(pdir,"inc_prog",sep="/")
tdir <- paste(cdir,"tools",sep="/")
othdir <- paste(pdir,"others",sep="/")
gdir <- "C:/GAMS/win64/26.1"

# Make input/output directory
if(!dir.exists(odir)){dir.create(odir,recursive=T)}
if(!dir.exists(ddir)){dir.create(ddir,recursive=T)}

reg_in <- "World"
year_all <- seq(2005,2100,by=5)

igdx(gdir)



# Region mapping ----------------------------------------------------------

R230_106 <- read_delim(paste0(ddir,"/230_106.map"),delim=" . ",comment="*",col_names=FALSE) %>% rename(R230=1,R106=2)
R106_32 <- read_delim(paste0(ddir,"/R106_32.map"),delim=" . ",comment="*",col_names=FALSE) %>% rename(R106=1,R33=2)
R106_17 <- read_delim(paste0(ddir,"/region17.map"),delim=".",col_names=FALSE) %>% rename(R106=1,R17=2) %>% mutate(across(starts_with("R"),~str_replace_all(.,"\t","")))
RegionMap <- R230_106 %>% left_join(R106_32) %>% left_join(R106_17) %>% drop_na()
R33_17 <- RegionMap %>% select(R33,R17) %>% distinct(R33,.keep_all=TRUE)

# Input data --------------------------------------------------------------

Fer_Use_CGE <- rgdx.param(paste0(ddir,'/global_17_IAMC.gdx'),'IAMC_Template') %>% 
  filter(SCENARIO=="SSP2_BaU_NoCC",VEMF=="Fer_Use_Nit") %>% select(-SCENARIO) %>% 
  pivot_wider(names_from=YEMF,values_from=IAMC_Template) %>% slice(1:17) %>% 
  pivot_longer(cols=-c(REMF,VEMF),names_to="Y",values_to="Vc") %>% 
  mutate(Y=as.numeric(Y)) %>% rename(R17=REMF,Variable=VEMF) %>%
  select(-Variable) %>% rename(Fer_Use_Nit=Vc)

Fer_Use_FAO <- read_csv(paste0(ddir,'/FAOSTAT_data_221116.csv')) %>%
  select(3,6,8,10,12) %>% filter(Item=='Nutrient nitrogen N (total)') %>% select(-Item) %>%
  rename(ISO3=1,Variable=2,Y=3,Vf=4) %>%
  mutate(Variable=recode(Variable,Production='Fer_Pro_Nit',
                         `Import Quantity`='Fer_Imp_Nit',
                         `Export Quantity`='Fer_Exp_Nit',
                         `Agricultural Use`='Fer_Use_Nit')) %>%
  replace_na(list(Vf=0)) %>% 
  pivot_wider

Fer_Use_FAO_Y5 <- Fer_Use_FAO %>%
  filter(Year)

hoge <- Fer_Use_FAO %>%
  left_join(RegionMap,by=c('ISO3'='R230')) %>%
  filter(R17=="XAF") %>%
  filter(Y>2000) %>%
  pivot_wider(names_from=Variable,values_from=Vf) %>%
  select(-3,-4,-5) %>%
  filter(Y%in%seq(2005,2020,5))

Fer_Use <- Fer_Use_FAO %>%
  full_join(Fer_Use_CGE) %>% filter(Y>=2005) %>% drop_na(R33,Vc,Vf) %>%
  group_by(Y,R17) %>%
  summarise(Vf=sum(Vf),Vc=sum(Vc)*10^6) %>%
  mutate(hoge=(Vc-Vf)/Vf)


# Linear interpolation ----------------------------------------------------

ind_t <- rgdx.param(paste0(ddir,"/serv_global_SSP2.gdx"),"ind_t") %>% 
  rename("R33"=1,"Variable"=2,"Y"=3,"Value"=4) %>% 
  filter(Variable!="GDP_PPP") %>% 
  mutate(across(where(is.factor),~as.character(.))) %>% 
  mutate(Y=as.numeric(Y)) %>%
  pivot_wider(names_from=Variable,values_from=Value) %>% 
  select(-GDP_MER) %>% 
  filter(Y%in%seq(2005,2100,5))

Fer_Use <- ind_t %>% 
  left_join(R33_17) %>%
  left_join(Fer_Use_CGE) %>% 
  group_by(Y,R17) %>% 
  mutate(Fer_Use_Nit=Fer_Use_Nit*POP/sum(POP)) %>%
  ungroup() %>% select(-R17,-POP)



Fer_Use_Y1 <- Fer_Use %>%
  group_by(R33) %>% nest() %>% 
  mutate(data=map(data,~{
    approx(.$Y,.$Fer_Use_Nit,xout=c(2005:2100)) %>% bind_rows()
  })) %>% 
  unnest(data) %>% 
  rename(Year=x,Value=y)

  


g <- Fer_Use_Y1 %>%
  group_by(Year) %>%
  summarise(Value=sum(Value)) %>% 
  ggplot() +
  geom_path(aes(x=Year,y=Value)) +
  labs(y='Nitrogen fertilizer consumption (Mt-N/yr)')
plot(g)

ggsave(paste0(odir,'/NitrogenFertilizerDemand.png'),g,width=6,height=5)
