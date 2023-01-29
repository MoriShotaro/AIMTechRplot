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


# HVC ---------------------------------------------------------------------

HVC_feedshare <- R33_17 %>% 
  select(R33)

NH3_feedshare <- R33_17 %>% 
  select(R33) %>%
  mutate(N_NH3_NGS=case_when(R33=='CHN' ~ 0.14,
                       TRUE ~ 1,
                       ),
         N_NH3_COL=case_when(R33=='CHN' ~ 0.86,
                       TRUE ~ 0))# MOH

MOH_feedshare <- R33_17 %>% 
  select(R33) %>% 
  mutate(N_MOH_NGS=case_when(R33=='CHN' ~ 0.2,
                       TRUE ~ 1),
         N_MOH_COL=case_when(R33=='CHN' ~ 0.8,
                       TRUE ~ 0))

HVC_feedshare <- R33_17 %>% 
  select(R33) %>% 
  mutate(N_HVC_OIL=case_when(R33=='JPN' ~ 0.96,
                       R33=='USA' ~ 0.38,
                       R33=='BRA' ~ 0.99,
                       R33=='CAN' ~ 0.11,
                       R33=='CHN' ~ 1,
                       R33=='IND' ~ 0.68,
                       R33=='KOR' ~ 1,
                       R33%in%c('XE15','XE10','XE3') ~ 0.86,
                       TRUE ~ 0.52),
         N_HVC_GAS=case_when(R33=='JPN' ~ 0.04,
                       R33=='USA' ~ 0.62,
                       R33=='BRA' ~ 0.01,
                       R33=='CAN' ~ 0.89,
                       R33=='CHN' ~ 0,
                       R33=='IND' ~ 0.32,
                       R33=='KOR' ~ 0,
                       R33%in%c('XE15','XE10','XE3') ~ 0.14,
                       TRUE ~ 0.48))


lstHVC <- wgdx.reshape(HVC_feedshare,symDim=2,symName="feedshare",tName="L")
lstNH3 <- wgdx.reshape(NH3_feedshare,symDim=2,symName="feedshare",tName="L")
lstMOH <- wgdx.reshape(MOH_feedshare,symDim=2,symName="feedshare",tName="L")

wgdx.lst(paste0(odir,"/HVC_feedshare.gdx"),lstHVC)
wgdx.lst(paste0(odir,"/NH3_feedshare.gdx"),lstNH3)
wgdx.lst(paste0(odir,"/MOH_feedshare.gdx"),lstMOH)


