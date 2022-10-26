# Library -----------------------------------------------------------------

library(tidyverse)
library(gdxrrw)
library(stringr)
library(data.table)


# Make output directory ---------------------------------------------------

if(!dir.exists(paste(odir,"ppt",sep="/"))){dir.create(paste(odir,"ppt",sep="/"),recursive=T)}

# Input data --------------------------------------------------------------

df_baseline <- df_all %>% 
  filter(Scenario%in%c("Baseline","Baseline_w/o_CTLGTL"))

dfAR6 <- fread(paste0(tdir,"/AR6/output/AR6_Scenario_Database_Enduse.csv"),stringsAsFactors=FALSE,header=TRUE) %>% 
  pivot_longer(cols=c(Y2010:Y2100),names_to="Year",values_to="Value",names_prefix="Y") %>%
  mutate(Year=as.numeric(Year)) %>% 
  drop_na(Value)

# Function ----------------------------------------------------------------

fplot_areab <- function(var, totvar=NA, ylabel, w=150, h=100, cpt=NULL, name_file, adj_position='stack'){
  dt <- filter(df_baseline, Variable %in% var$Variable) %>%
    mutate(Scenario=factor(Scenario, levels=scen_mat$Scenario)) %>%
    mutate(cat1=scen_mat$scen_cpol[Scenario], cat2=scen_mat$scen_techpol[Scenario]) %>%
    mutate(cat1=factor(cat1, levels=unique(scen_mat$scen_cpol))) %>%
    mutate(cat2=factor(cat2, levels=unique(scen_mat$scen_techpol))) %>%
    mutate(Variable=factor(Variable, levels=rev(var$Variable))) %>%
    filter(Scenario!='historical')
  plt$color <- as.character(var$Color); names(plt$color) <- as.character(var$Variable)
  plt$legend <- as.character(var$Legend); names(plt$legend) <- as.character(var$Variable)
  if(nrow(dt)>0){
    p <- ggplot(data=dt)+
      geom_area(aes(x=Year, y=Value, fill=Variable), position=adj_position, show.legend=T, alpha=1)+
      scale_fill_manual(values=plt$color, labels=plt$legend, name="Variable")
    if(all(is.na(scen_mat$scen_techpol))){
      p <- p + facet_grid(cat1~.)
    } else {
      p <- p + facet_grid(cat1~cat2)}
    p <- p+labs(title=cpt, x=NULL, y=ylabel)+
      theme_bw()+
      theme(legend.position='bottom', legend.title=element_blank(), 
            axis.text.x=element_text(angle=45, hjust=1),
            strip.background=element_blank(),
            panel.grid.minor=element_blank())+
      MyThemeLine
    if(name_file!=F){
      ggsave(filename=paste0(odir,"/ppt/",name_file,".png"), plot=p, width=w, height=h, units='mm', dpi=300)
    }
    return(p)
  } else {
    print('error: empty data frame')
  }
}

fplot_fbarb <- function(var, ylabel, w=150, h=100, cpt=NULL, name_file, adj_position='fill'){
  dt <- filter(df_baseline, Variable %in% var$Variable) %>%
    mutate(Scenario=factor(Scenario, levels=scen_mat$Scenario)) %>%
    mutate(cat1=scen_mat$scen_cpol[Scenario], cat2=scen_mat$scen_techpol[Scenario]) %>%
    mutate(cat1=factor(cat1, levels=unique(scen_mat$scen_cpol))) %>%
    mutate(cat2=factor(cat2, levels=unique(scen_mat$scen_techpol))) %>%
    mutate(Variable=factor(Variable, levels=rev(var$Variable))) %>%
    filter(Scenario!='historical')
  plt$color <- as.character(var$Color); names(plt$color) <- as.character(var$Variable)
  plt$legend <- as.character(var$Legend); names(plt$legend) <- as.character(var$Variable)
  if(nrow(dt)>0){
    p <- ggplot(data=dt)+
      geom_bar(aes(x=Year, y=Value, fill=Variable), stat="identity", position=adj_position, show.legend=T, alpha=1)+
      scale_fill_manual(values=plt$color, labels=plt$legend, name="Variable")+
      scale_y_continuous(labels=percent)
    if(all(is.na(scen_mat$scen_techpol))){
      p <- p + facet_grid(.~cat1)
    } else {
      p <- p + facet_grid(cat1~cat2)}
    p <- p+labs(title=cpt, x=NULL, y=ylabel)+
      theme_bw()+
      theme(legend.position='bottom', legend.title=element_blank(), 
            axis.text.x=element_text(angle=45, hjust=1),
            strip.background=element_blank(),
            panel.grid.minor=element_blank())+
      MyThemeLine
    if(name_file!=F){
      ggsave(filename=paste0(odir,"/ppt/",name_file,".png"), plot=p, width=w, height=h, units='mm', dpi=300)
    }
    return(p)
  } else {
    print('error: empty data frame')
  }
}

fplot_pathb <- function(var, ylabel, ylim=c(0,NA), ylab=waiver(), w=150, h=100, cpt=NULL, name_file){
  dt <- filter(df_baseline, Variable %in% var) %>%
    mutate(Scenario=factor(Scenario, levels=scen_mat$Scenario)) %>%
    filter(Scenario!='historical')
  if(nrow(dt)>0){
    p <- ggplot(data=dt)+
      geom_path(aes(x=Year, y=Value, color=Scenario), show.legend=T, alpha=1)+
      labs(title=cpt, x=NULL, y=ylabel)+
      scale_y_continuous(limits=ylim, labels=ylab)
    if(length(var) > 1){
      p <- p+facet_wrap(~Variable,scales='free_y')
    }
    p <- p+theme_bw()+
      theme(legend.position='right', strip.background=element_blank(),panel.grid.minor=element_blank(),
            axis.text.x=element_text(angle=45, hjust=1))+
      MyThemeLine
    if(name_file!=F){
      ggsave(filename=paste0(odir,"/ppt/",name_file,".png"), plot=p, width=w, height=h, units='mm', dpi=300)
    }
    return(p)
  } else {
    print('error: empty data frame')
  }
}

fplot_linepathb <- function(var,scen,bname=unique(dfAR6$Name),bmodel=unique(dfAR6$Model),bscen=unique(dfAR6$Scenario),bcat=unique(dfAR6$category),byear,ylabel,ylim=NULL,ylab=waiver(),w=150,h=100,cpt=NULL,name_file,labels=FALSE){
  dt <- df_all %>% 
    filter(Variable %in% var) %>%
    mutate(Scenario=factor(Scenario, levels=scen_mat$Scenario)) %>%
    filter(Scenario!='historical') %>% 
    filter(Scenario%in%scen)
  dt2 <- dfAR6 %>% 
    filter(Variable%in%var) %>%
    filter(Name%in%bname,Model%in%bmodel,Scenario%in%bscen,category%in%bcat) %>% 
    filter(Yend==2100)
  if(nrow(dt)>0){
    p <- ggplot()+
      geom_line(data=dt2, aes(x=Year,y=Value,group=Name),size=0.5, color="grey70")+
      geom_path(data=dt, aes(x=Year, y=Value, color=Scenario), show.legend=T, alpha=1, size=0.5)+
      labs(title=cpt, x=NULL, y=ylabel, color="Scenario")+
      scale_y_continuous(limits=c(0,NA), labels=ylab)
    if(length(var) > 1){
      p <- p+facet_wrap(~Variable,scales='free_y')
    }
    if(labels){
      p <- p+geom_text(data=dt2%>%filter(Year==2100),aes(x=Year,y=Value,label=Name),size=2.5,hjust=1.2,check_overlap=TRUE)
    }
    p <- p+theme_bw()+
      theme(legend.position='right', strip.background=element_blank(),panel.grid.minor=element_blank(),
            axis.text.x=element_text(angle=45, hjust=1))+
      MyThemeLine
    if(name_file!=F){
      ggsave(filename=paste0(odir,"/ppt/",name_file,".png"), plot=p, width=w, height=h, units='mm', dpi=300)
    }
    return(p)
  } else {
    print('error: empty data frame')
  }
}


# Scenario selection ------------------------------------------------------

# nmls <- c("MESSAGE-GLOBIOM 1.0|SSP2-Baseline","AIM/Hub-Global 2.0|Baseline","WITCH-GLOBIOM 3.1|SSP2-Baseline",
#           "IMAGE 3.2|SSP2-baseline","REMIND-MAgPIE 1.5|SSP2-Baseline")
nmls <- c("MESSAGE-GLOBIOM 1.0|SSP2-Baseline","AIM/Hub-Global 2.0|Baseline")
scls <- c("CD-LINKS_INDCi","CO_CurPol","EN_NPi2100","Baseline","EN_INDCi2100","CD-LINKS_NPi","EMF30_Baseline","SSP2-Baseline",
          "SSP2-baseline","EMF33_Baseline")
mdls<- c("AIM/CGE 2.1","AIM/CGE2.2","AIM/Hub-Global 2.0")
ctls<- c()


# Primary energy ----------------------------------------------------------

fplot_areab(var=prmene, totvar='Prm_Ene', name_file='PES', ylabel='Primary energy supply (EJ/yr)', cpt='', w=150, h=100)
fplot_fbarb(var=prmene, name_file='PES2', ylabel='Share in Primary energy (%)', cpt='', w=150, h=100)
fplot_pathb(var='Prm_Ene', name_file='PES_path', ylabel='Primary energy supply (EJ/yr)', cpt='', w=150, h=100)


# Final energy ------------------------------------------------------------

fplot_areab(var=finene, totvar='Fin_Ene', name_file='FEC', ylabel='Final energy consumption (EJ/yr)', cpt='', w=150, h=100)
fplot_fbarb(var=finene, name_file='FEC2', ylabel='Share in final energy (%)', cpt='', w=150, h=100)
fplot_pathb(var='Fin_Ene', name_file='FEC_path', ylabel='Final energy consumption (EJ/yr)', cpt='', w=150, h=100)

# Emission ----------------------------------------------------------------

fplot_linepathb("Emi_CO2_Ene_and_Ind_Pro",scen=c("Baseline","Baseline_w/o_CTLGTL"),bname=nmls,name_file="Emi_CO2_FFI",
                ylabel=expression(paste(CO[2],' emissions (Mt-',CO[2],'/yr)')),cpt="Emi_CO2_FFI",labels=T, w=150, h=100)


# Oil composition ---------------------------------------------------------

source(paste(pdir2,"OilComposition.R",sep="/"))
fplot_pathb(var='Sec_Ene_Liq_Oil', name_file='Oil_path', ylabel='Oil Product Production (EJ/yr)', cpt='', w=150, h=100)
fplot_pathb(var='Fin_Ene_Liq_Oil', name_file='Oil_path', ylabel='Oil Product Production (EJ/yr)', cpt='', w=150, h=100)

fplot_path(var='Prc_Car', name_file='Cost_Price_carbon', ylabel=expression(paste('Carbon price (US$/t-',CO[2],')')), cpt='Prc_Carbon')
