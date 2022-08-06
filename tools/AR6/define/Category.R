#library--------------------------------------------------------------------

library(gdxrrw)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(cowplot)
library(fs)
library(readxl)
library(data.table)

# input data --------------------------------------------------------------

for (i in 1:8){
  category_name<-paste0('C',i)
  category_data<-paste0('../data/scenario_category/C',i,'.csv')
  data<-fread(category_data,stringsAsFactors=FALSE,header=TRUE) %>% 
    select(1,2) %>% 
    unite(col=Name,Model,Scenario,sep="|") %>% 
    mutate(category=category_name)
  assign(category_name,data)
}

scen_category <- bind_rows(C1,C2,C3,C4,C5,C6,C7,C8) %>%
  mutate(category=factor(category,levels=c("C1","C2","C3","C4","C5","C6","C7","C8")))
  
