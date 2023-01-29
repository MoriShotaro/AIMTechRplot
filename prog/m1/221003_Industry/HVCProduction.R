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
library(gdxdt)

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


# Input data --------------------------------------------------------------

# Read original data without column names
df_ebal <- readLines(paste0(ddir,"/WBAL_TJ_2013.csv")) %>% 
  str_replace_all('",',"',") %>% 
  str_replace_all('^"|"$', "'") %>%
  str_replace_all(',"', ",'") %>% 
  paste(collapse='\n') %>% 
  fread(quote="'",skip=2) %>% 
  filter(V1>=2000)

# Read and join column names
cols <- readLines(paste0(ddir,"/WBAL_TJ_2013.csv")) %>% 
  str_replace_all('",',"',") %>% 
  str_replace_all('^"|"$', "'") %>%
  str_replace_all(',"', ",'") %>%
  str_replace_all(' ', "") %>%
  paste(collapse='\n') %>%
  fread(quote="'",nrow=1,fill=TRUE) %>% unlist()

names(cols)[[1]] <- "FLOW"

colnames(df_ebal) <- names(cols)

df <- df_ebal %>%
  slice(1:31,49:106) %>% 
  pivot_longer(cols=-c(FLOW),names_to="PRODUCT",values_to="VALUE") %>% 
  mutate(VALUE=str_replace_all(VALUE,"\\.\\.","0")) %>% 
  mutate(VALUE=str_replace_all(VALUE,"c","0")) %>%
  mutate(VALUE=str_replace_all(VALUE,"x","0")) %>% 
  mutate(VALUE=as.numeric(VALUE)) %>%
  pivot_wider(names_from=PRODUCT,values_from=VALUE)

df_Ethane <- df %>% 
  select(FLOW,Ethane) %>% 
  filter(Ethane!=0)

df_nonChem <- df %>% 
  filter(FLOW=="Memo: Non-energy use in chemical/petrochemical") %>%
  pivot_longer(cols=-c(FLOW),names_to="PRODUCT",values_to="Value") %>% 
  select(-FLOW) %>% 
  filter(Value!=0,PRODUCT!="Total") %>%
  mutate(Value=Value/1000,Share=Value/sum(Value)) %>% 
  arrange(-Share) %>% 
  mutate(Flag=c("O","G","L","O","O","O","O","O","O","O","C","Cr","C","O","O","C","O","O","O","C","C","O","O","C")) %>%
  group_by(Flag) %>% 
  summarise(Value=sum(Value))

df_demChemNGS <- df_demChem %>%
  group_by(Y,Feedstock,Product) %>% 
  summarize(Value=sum(Value)) %>%
  filter(Y==2013,Product=="Total")
