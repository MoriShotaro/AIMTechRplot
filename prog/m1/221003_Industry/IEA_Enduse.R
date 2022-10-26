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

# Japan(COUNTRY) to JAPAN(IEA) mapping
iea_country <- read.delim(paste0(ddir,"/ieacountry.set"),header=FALSE,sep="\t") %>%
  rename(IEA=1,COUNTRY=2)

# JAPAN(IEA) to JPN(R32) mapping
iea_R32 <- read.delim(paste0(ddir,"/iea32.map"),header=FALSE,sep=".") %>% 
  rename(IEA=1,R32=2) %>% 
  mutate(IEA=str_replace_all(IEA," ","")) %>%
  mutate(IEA=str_replace_all(IEA,"\t",""))

# Japan(COUNTRY) to JPN(R32) mapping
country_R32 <- full_join(iea_country,iea_R32) %>%
  select(COUNTRY,R32) %>% 
  mutate(COUNTRY=recode(COUNTRY,"China (P.R. of China and Hong Kong  China)"="China (P.R. of China and Hong Kong, China)"))

# Read 32 region and 23 region
R32 <- iea_R32 %>% distinct(R32) %>% filter(R32!="INTL")
R23 <- readxl::read_xls(paste0(ddir,"/IEA_EneBal_2005_1012.xls"),sheet="地域コード",skip=4) %>%
  distinct(AIM_GLB) %>% rename(R23=AIM_GLB)

# R23 to R15 mapping
R23_R15 <- read.delim(paste0(ddir,"/R23_15.set"),header=FALSE,sep=" ") %>% 
  rename(R23=1,R15=2)

# R32 to R23 mapping
R32_R23 <- read.delim(paste0(ddir,"/R32_23.set"),header=FALSE,sep=" ") %>% 
  rename(R32=1,R23=2)

# R32 to R15 mapping
R32_R15 <- R32_R23 %>% full_join(R23_R15) %>% select(-R23)


# Product and flow mapping ------------------------------------------------

# Flow mapping
flow_map <- read.delim(paste0(ddir,"/flow_iea.set"),header=FALSE,sep="\t") %>% 
  select(-V3) %>% rename(FLOW_CODE=V1,FLOW=V2)

# Product mapping
prod_map <- read.delim(paste0(ddir,"/product_iea.set"),header=FALSE,sep="\t") %>% 
  rename(PROD_CODE=V1,PRODUCT=V2) %>% 
  bind_rows(data.frame(PROD_CODE=c("COAL","OIL","COMRENEW"),PRODUCT=c("COAL","OIL","COMRENEW")))


# Flow and product list ---------------------------------------------------

flow_ind <- c("IRONSTL","CHEMICAL","NONFERR","NONMET","PAPERPRO","TOTIND")
prod_ind <- c("COAL","OIL","NATGAS","COMRENEW","ELECTR","HEAT")


# Input IEA Energy Balance ------------------------------------------------

# Read original data without column names
df_ebal <- readLines(paste0(ddir,"/WBAL_ktoe.csv")) %>% 
  str_replace_all('",',"',") %>% 
  str_replace_all('^"|"$', "'") %>%
  str_replace_all(',"', ",'") %>% 
  paste(collapse='\n') %>% 
  fread(quote="'",skip=2) %>% 
  filter(V1>=2000)

# Read and join column names
cols <- fread(paste0(ddir,"/WBAL_ktoe.csv"),nrow=1) %>% unlist()
cols[[1]] <-  'TIME'; cols[[2]] <- 'FLOW'; cols[[3]] <-  'COUNTRY'
colnames(df_ebal) <- cols


# Input other data --------------------------------------------------------

# Final energy consumption in Cement sector from AIM/Technology
df_cement <- rgdx.param(paste0(ddir,"/merged_output.gdx"),"iamc_gdx") %>%
  filter(Sv%in%c("Fin_Ene_Ind_Cem_SolidsFos",
                 "Fin_Ene_Ind_Cem_Liq",
                 "Fin_Ene_Ind_Cem_Gas",
                 "Fin_Ene_Ind_Cem_SolidsBio",
                 "Fin_Ene_Ind_Cem_Ele"),
         Y5==2005, Sc=="Baseline") %>% 
  mutate(Sv=recode(Sv,"Fin_Ene_Ind_Cem_SolidsFos"="COAL",
                   "Fin_Ene_Ind_Cem_Liq"="OIL",
                   "Fin_Ene_Ind_Cem_Gas"="NATGAS",
                   "Fin_Ene_Ind_Cem_SolidsBio"="COMRENEW",
                   "Fin_Ene_Ind_Cem_Ele"="ELECTR")) %>% 
  select(-Sc,-Y5) %>% filter(Sr!="XOC") %>% 
  rename(R32=1,PROD_CODE=2,VALUE=3) %>% 
  mutate(VALUE=VALUE*10^6/41.868) %>% 
  pivot_wider(names_from=PROD_CODE,values_from=VALUE) %>%
  mutate(R32=as.character(R32)) %>% 
  mutate(FLOW_CODE=rep("CEMENT"),COMRENEW=rep(NA),HEAT=rep(NA)) %>%
  replace(is.na(.),0)

# Breakout by energy service (Fraction Shares)
R15 <- R23_R15 %>% distinct(R15)
  
df_srvsh <- data.frame(R15) %>% 
  mutate(data=map(R15,~readxl::read_xls(paste0(ddir,"/IEA_EneBal_2005_1012.xls"),sheet=.,skip=3) %>%
                    slice(-seq(8,48,8)) %>% replace_na(list(Industry="Other Industrial")) %>% replace(is.na(.),0) %>% 
                    rename("FLOW_CODE"=1,"SERVICE"=3,"ELECTR"=4,"NATGAS"=5,"COAL"=8,"OIL"=13,"COMRENEW"=18,"HEAT"=20) %>% 
                    select("FLOW_CODE","SERVICE","ELECTR","NATGAS","COAL","OIL","COMRENEW","HEAT") %>% 
                    mutate(FLOW_CODE=recode(FLOW_CODE,
                                            "Iron and Steel"="IRONSTL",
                                            "Non-ferrous Metals"="NONFERR",
                                            "Chemicals and Petrochemicals"="CHEMICAL",
                                            "Paper, Pulp and Printing"="PAPERPRO",
                                            "Non-Metallic minerals (SCG)"="NONMET",
                                            "Other Industrial"="OTHER"
                                            )))) %>% 
  unnest(data) %>% 
  group_by(R15,FLOW_CODE) %>% 
  nest() %>% 
  rename(SRV=data) %>% 
  mutate(SRV=map(SRV,~pivot_longer(.,cols=-c(SERVICE),names_to="PROD_CODE",values_to="SHARE") %>% 
                   select(PROD_CODE,SERVICE,SHARE)))


# Arrange data ------------------------------------------------------------

# Arrange original data frame
df <- df_ebal %>%
  left_join(country_R32) %>% filter(!is.na(R32),R32!="INTL",TIME==2005) %>%
  select(-COUNTRY,-TIME) %>% 
  select(FLOW,R32,everything()) %>%
  pivot_longer(cols=-c(FLOW,R32),names_to="PRODUCT",values_to="VALUE") %>%
  mutate(VALUE=str_replace_all(VALUE,"\\.\\.","0")) %>% 
  mutate(VALUE=str_replace_all(VALUE,"c","0")) %>%
  mutate(VALUE=str_replace_all(VALUE,"x","0")) %>%
  mutate(VALUE=as.numeric(VALUE)) %>%
  group_by(FLOW,PRODUCT,R32) %>%
  summarise(VALUE=sum(VALUE)) %>%
  ungroup() %>% 
  pivot_wider(names_from=PRODUCT,values_from=VALUE) %>%
  mutate(FLOW=str_replace_all(FLOW,"            Paper, pulp and printing",
                              "Paper  pulp and print")) %>% 
  mutate(COAL=`Coal and coal products`+`Peat and peat products`,
         OIL=`Oil shale and oil sands`+`Crude, NGL and feedstocks`+`Oil products`,
         COMRENEW=`Biofuels and waste`) %>%
  pivot_longer(cols=-c(FLOW,R32),names_to="PRODUCT",values_to="VALUE") %>% 
  left_join(flow_map) %>% left_join(prod_map) %>% 
  select(FLOW_CODE,R32,PROD_CODE,VALUE) %>% 
  filter(FLOW_CODE%in%flow_ind,PROD_CODE%in%prod_ind) %>%
  pivot_wider(names_from=FLOW_CODE,values_from=VALUE) %>% 
  mutate(OTHER=TOTIND-(CHEMICAL+IRONSTL+NONFERR+NONMET)) %>% 
  select(-TOTIND) %>%
  pivot_longer(cols=-c(PROD_CODE,R32),names_to="FLOW_CODE",values_to="VALUE") %>% 
  pivot_wider(names_from=PROD_CODE,values_from=VALUE) %>%
  bind_rows(df_cement) %>% 
  pivot_longer(cols=-c(R32,FLOW_CODE),names_to="PROD_CODE",values_to="VALUE") %>% 
  pivot_wider(names_from=FLOW_CODE,values_from=VALUE) %>% 
  mutate(NONMET=NONMET-CEMENT) %>% select(-CEMENT) %>% 
#  mutate(NONMET=ifelse(NONMET<0,0,NONMET)) %>%
  pivot_longer(cols=-c(R32,PROD_CODE),names_to="FLOW_CODE",values_to="VALUE") %>%
  group_by(R32,FLOW_CODE) %>% 
  nest() %>%
  full_join(R32_R15) %>% full_join(df_srvsh) %>%
  drop_na(R15) %>% select(-R15) %>%
  mutate(data=map2(data,SRV,~full_join(.x,.y))) %>% 
  select(-SRV) %>% 
  mutate(data=map(data,~mutate(.,VALUE=VALUE*SHARE) %>% 
                    select(PROD_CODE,SERVICE,VALUE) %>% 
                    group_by(SERVICE) %>% 
                    summarize(VALUE=sum(VALUE)))) %>% 
  filter(FLOW_CODE%in%c("NONFERR","NONMET","OTHER")) %>%
  unnest(data) %>% 
  group_by(R32,SERVICE) %>% 
  summarize(VALUE=sum(VALUE))
  


  