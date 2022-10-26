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


# Input data --------------------------------------------------------------

# Neelis et al. (2005), Table5
df_HVC_Neelis <- read_csv(paste0(ddir,"/process_energy.csv")) %>% 
  mutate(Naphtha=Naphtha*1000/1001,`Gas oil`=`Gas oil`*1000/999) %>%
  filter(Product!="Total") %>% 
  pivot_longer(cols=-c(Product,Flow),names_to="Feedstock",values_to="Value") %>% 
  group_by(Flow,Feedstock) %>% 
  summarise(Production=sum(Value)) %>%
  filter(Flow=="H") %>% 
  mutate(GFR1=1000*1000/Production) %>%
  bind_cols(LHV=c(49.3,51.9,45.94,50.3,46.01)) %>%  # METI(2020), MJ/kg
  mutate(GFR2=GFR1/1000*LHV)
  pivot_wider(names_from=Feedstock,values_from=Value)
