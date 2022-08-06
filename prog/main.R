# Option ------------------------------------------------------------------

general <- TRUE
index <- TRUE

# library -----------------------------------------------------------------

library(tidyverse)
library(gdxrrw)
library(stringr)
library(patchwork)
library(readr)
library(scales)


# Setting -----------------------------------------------------------------

# General information
date <- "2208041109"
project <- "model_expansion"
cdir <- getwd()
odir <- paste(cdir,"output",project,date,sep="/")
ddir <- paste(cdir,"data",project,date,"main",sep="/")
pdir <- paste(cdir,"prog",sep="/")
mdir <- paste(pdir,"inc_prog",sep="/")
tdir <- paste(cdir,"tools",sep="/")
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


# Load sub-programs
source(paste(mdir,"dataframe.R",sep="/"))
source(paste(mdir,"function.R",sep="/"))

plt <- list()


# Plot --------------------------------------------------------------------

if(general){source(paste(mdir,"general.R",sep="/"))}
if(index){source(paste(mdir,"index.R",sep="/"))}