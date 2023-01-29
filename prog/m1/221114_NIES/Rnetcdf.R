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
library(ncdf4)

# Setting -----------------------------------------------------------------

# General information
date <- "m1"
project <- "221104_NIES"
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

nc <- nc_open(paste0(ddir,"/Wind_WFDE5_CRU_201808_v1.1.nc"))



# Data arrange ------------------------------------------------------------

Wind <- ncvar_get(nc,"Wind")
hoge <- Wind[,,1]
fuga <- Wind[1,1,]
Wind[is.na(Wind)] <- 0

date <- 1:31 %>% map(function(i){
  date[i] <- Wind[,,(1+24*(i-1)):(24*i)] %>% apply(3,mean)
})


skimr::skim(Wind)


