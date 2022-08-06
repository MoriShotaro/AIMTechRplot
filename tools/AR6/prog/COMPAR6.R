# Library -----------------------------------------------------------------

library(tidyverse)
library(gdxrrw)
library(stringr)
library(data.table)


# Input data --------------------------------------------------------------

dfAR6 <- fread(paste0(tdir,"/AR6/output/AR6_Scenario_Database.csv"),stringsAsFactors=FALSE,header=TRUE) %>% 
  pivot_longer(cols=as.character(seq(2010,2100,5)),names_to="Variable",values_to="Value")

# Functions ---------------------------------------------------------------

fplot_boxpath <- function(var, scen, bscen, byear, ylabel, ylim=NULL, ylab=waiver(), w=150, h=100, cpt=NULL, name_file){
  dt <- df %>% 
    filter(Variable %in% var) %>%
    mutate(Scenario=factor(Scenario, levels=scen_mat$Scenario)) %>%
    filter(Scenario!='historical') %>% 
    filter(Scenario%in%scen)
  dt2 <- dfAR6 %>% 
    filter(Variable %in% var, Year %in% byear) %>% 
    filter(Name %in% scen|Model %in% scen|Scenario %in% scen|Category %in% scen)
  if(nrow(dt)>0){
    p <- ggplot()+
      geom_path(data=dt, aes(x=Year, y=Value, color=Scenario), show.legend=T, alpha=1)+
      geom_boxplot(data=dt, aes(x=Year, y=Value))+
      labs(title=cpt, x=NULL, y=ylabel)+
      scale_y_continuous(limits=ylim, labels=ylab)
    if(length(var) > 1){
      p <- p+facet_wrap(~Variable,scales='free_y')
    }
    p <- p+theme_bw()+
      theme(legend.position='right', strip.background=element_blank(),panel.grid.minor=element_blank(),
            axis.text.x=element_text(angle=45, hjust=1))
    if(name_file!=F){
      ggsave(filename=paste0(odir,"/general/",name_file,".png"), plot=p, width=w, height=h, units='mm', dpi=300)
    }
    return(p)
  } else {
    print('error: empty data frame')
  }
}

fplot_sharepath <- function(var,totvar,scen,bscen,byear,ylabel,ylim=c(0,NA),ylab=waiver(),name_file,cpt=NULL){
  varlist <- str_c(totvar,var,sep="_")
  dt <-  df_all %>% 
    filter(Variable %in% c(totvar,varlist)) %>% 
    pivot_wider(names_from = Variable, values_from = Value) %>%
    mutate(across(c(starts_with(totvar)),~.x*100/eval(parse(text=totvar)))) %>% 
    select(-eval(totvar)) %>% 
    pivot_longer(cols=c(starts_with(totvar)),names_to="Variable",values_to="Value") %>% 
    mutate(Scenario=factor(Scenario, levels=scen_mat$Scenario)) %>%
    filter(Scenario!='historical') %>% 
    filter(Scenario%in%scen)
  dt2 <- dfAR6 %>% 
    filter(Variable %in% var, Year %in% byear) %>% 
    filter(Name %in% scen|Model %in% scen|Scenario %in% scen|Category %in% scen)
  w <- 250
  h <- ceiling(length(varlist)/3)*75
  if(nrow(dt)>0){
    p <- ggplot(data=dt)+
      geom_path(aes(x=Year, y=Value, color=Scenario), show.legend=T, alpha=1)+
      geom_boxplot(data=dt, aes(x=Year, y=Value))+
      labs(title=cpt, x=NULL, y=ylabel)+
      scale_y_continuous(limits=ylim, labels=ylab)
    if(length(var) > 1){
      p <- p+facet_wrap(~Variable,scales='free_y',ncol=3)
    }
    p <- p+theme_bw()+
      theme(legend.position='right', strip.background=element_blank(),panel.grid.minor=element_blank(),
            axis.text.x=element_text(angle=45, hjust=1))
    if(name_file!=F){
      ggsave(filename=paste0(odir,"/index/",name_file,".png"), plot=p, width=w, height=h, units='mm', dpi=300)
    }
    return(p)
  } else {
    print('error: empty data frame')
  }
}

var_fin <- c("Liq_Oil","SolidsCoa","Gas","Ele","Hyd")
fplot_sharepath(var_fin,"Fin_Ene",scen="500C",bscen="C1",byear=C(2050,2100),name_file="SHR_fin",ylabel="Share in final energy (%)",cpt="SHR_fin")
