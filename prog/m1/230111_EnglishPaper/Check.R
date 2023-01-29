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

# Setting -----------------------------------------------------------------

# General information
date <- "m1"
project <- "230111_EnglishPaper"
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


# Check bioenergy potential -----------------------------------------------

emax_t1 <- rgdx.param(paste0(ddir,'/2201281605/gams_output/gdx_primary/INDCi2030_500f_Biofueloff.gdx'),'emax_t1') %>%
  drop_na() %>%
  filter(ME=='WOR',str_detect(K,'CRN'),emax_t1!=Inf)


