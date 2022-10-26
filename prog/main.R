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
project <- "220909_Renewable"
cdir <- getwd()
odir <- paste(cdir,"output",date,project,"main",sep="/")
ddir <- paste(cdir,"data",date,project,"main",sep="/")
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
reg_all <- rgdx.set(paste(ddir,"merged_output.gdx",sep="/"),"Sr")
year_all <- seq(2005,2100,by=5)

igdx(gdir)


# Import ------------------------------------------------------------------

# Import model output data
df_all <- rgdx.param(paste(ddir,"merged_output.gdx",sep="/"),"iamc_gdx") %>% 
  rename(Scenario='Sc',Region='Sr',Variable='Sv',Year='Y5',Value='iamc_gdx') %>%
  pivot_wider(names_from=Year,values_from=Value,values_fill=0) %>% 
  pivot_longer(cols=-c(Scenario,Region,Variable),names_to="Year",values_to="Value") %>%
  mutate(Year=as.numeric(as.character(Year)),Region=as.character(Region),Scenario=as.character(Scenario)) %>% 
  filter(Region%in%reg_in)

# Import scenario data
scen_mat <- read_csv(paste(ddir,"scenario_table.csv",sep="/"))
if(bind_baseline){source(paste(pdir2,"BindwoCTLGTL.R",sep="/"))}

# Load sub-programs
source(paste(mdir,"dataframe.R",sep="/"))
source(paste(mdir,"function.R",sep="/"))

plt <- list()


# Plot --------------------------------------------------------------------

if(general){source(paste(mdir,"general.R",sep="/"))}
if(index){source(paste(mdir,"index.R",sep="/"))}