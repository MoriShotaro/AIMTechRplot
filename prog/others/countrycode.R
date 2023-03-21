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
library(lemon)
library(cowplot)
library(maps)

# Setting -----------------------------------------------------------------

# General information
date <- "m1"
project <- "220909_Renewable"
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
year_all <- seq(2020,2050,by=5)

igdx(gdir)


# Input data --------------------------------------------------------------

R106 <- rgdx.set(paste0(ddir,'/winsol_grid.gdx'),'R106') %>% transmute(R106=as.character(i)) %>% 
  mutate(R106=str_remove_all(R106," "))
R106_32 <- read.delim(paste0(ddir,'/R106_32.map'),sep='.',header=FALSE) %>% 
  rename(R106=1,R32=2) %>% 
  mutate(R106=str_remove_all(R106,' ')) %>% mutate(R106=str_remove_all(R106,'\\*'))

df <- R106 %>% 
  full_join(R106_32)

countrycodes <- countrycode::codelist

