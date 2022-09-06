# Functions ---------------------------------------------------------------

func_fillY5 <- function(dt){
  df_fill <- dt %>%
    pivot_wider(names_from=Year,values_from=Value) %>%
    pivot_longer(cols=as.character(seq(2010,2100,5)),names_to="Year",values_to="Value") %>%
    mutate(Value2=Value) %>% 
    fill(Value) %>% 
    fill(Value2,.direction="up") %>% 
    mutate(Value=(Value+Value2)/2) %>% 
    select(-Value2)
  return(df_fill)
}


fplot_boxpath <- function(var,scen,bname=unique(dfAR6$Name),bmodel=unique(dfAR6$Model),bscen=unique(dfAR6$Scenario),bcat=unique(dfAR6$category),byear,ylabel,ylim=NULL,ylab=waiver(),w=150,h=100,cpt=NULL,name_file){
  dt <- df_all %>% 
    filter(Variable %in% var) %>%
    mutate(Scenario=factor(Scenario, levels=scen_mat$Scenario)) %>%
    filter(Scenario!='historical') %>% 
    filter(Scenario%in%scen)
  dt2 <- dfAR6 %>% 
    filter(Variable%in%var, Year %in% byear) %>%
    filter(Name%in%bname,Model%in%bmodel,Scenario%in%bscen,category%in%bcat)
  if(nrow(dt)>0){
    p <- ggplot()+
      geom_boxplot(data=dt2, aes(x=Year, y=Value, group=Year),width=5)+
      geom_path(data=dt, aes(x=Year, y=Value, color=Scenario), show.legend=T, alpha=1, size=1)+
      labs(title=cpt, x=NULL, y=ylabel)+
      scale_y_continuous(limits=ylim, labels=ylab)
    if(length(var) > 1){
      p <- p+facet_wrap(~Variable,scales='free_y')
    }
    p <- p+theme_bw()+
      theme(legend.position='right', strip.background=element_blank(),panel.grid.minor=element_blank(),
            axis.text.x=element_text(angle=45, hjust=1))
    if(name_file!=F){
      ggsave(filename=paste0(odir,"/AR6/",name_file,".png"), plot=p, width=w, height=h, units='mm', dpi=300)
    }
    return(p)
  } else {
    print('error: empty data frame')
  }
}

fplot_jitterpath <- function(var,scen,bname=unique(dfAR6$Name),bmodel=unique(dfAR6$Model),bscen=unique(dfAR6$Scenario),bcat=unique(dfAR6$category),byear,ylabel,ylim=NULL,ylab=waiver(),w=150,h=100,cpt=NULL,name_file,labels=FALSE){
  dt <- df_all %>% 
    filter(Variable %in% var) %>%
    mutate(Scenario=factor(Scenario, levels=scen_mat$Scenario)) %>%
    filter(Scenario!='historical') %>% 
    filter(Scenario%in%scen)
  dt2 <- dfAR6 %>% 
    filter(Variable%in%var, Year %in% byear) %>%
    filter(Name%in%bname,Model%in%bmodel,Scenario%in%bscen,category%in%bcat)
  if(nrow(dt)>0){
    p <- ggplot()+
      geom_jitter(data=dt2, aes(x=Year,y=Value,group=Year),width=0.5,size=1.5)+
      geom_path(data=dt, aes(x=Year, y=Value, color=Scenario), show.legend=T, alpha=1, size=0.5)+
      labs(title=cpt, x=NULL, y=ylabel, color="AIM/Technology")+
      scale_y_continuous(limits=ylim, labels=ylab)
    if(length(var) > 1){
      p <- p+facet_wrap(~Variable,scales='free_y')
    }
    if(labels){
      p <- p+geom_text(data=dt2,aes(x=Year,y=Value,label=Name),size=1.5,hjust=1.2,check_overlap=TRUE)
    }
    p <- p+theme_bw()+
      theme(legend.position='right', strip.background=element_blank(),panel.grid.minor=element_blank(),
            axis.text.x=element_text(angle=45, hjust=1))
    if(name_file!=F){
      ggsave(filename=paste0(odir,"/AR6/",name_file,".png"), plot=p, width=w, height=h, units='mm', dpi=300)
    }
    return(p)
  } else {
    print('error: empty data frame')
  }
}

fplot_boxshare <- function(var,totvar,scen,bname=unique(dfAR6$Name),bmodel=unique(dfAR6$Model),bscen=unique(dfAR6$Scenario),bcat=unique(dfAR6$category),byear,ylabel,ylim=c(0,NA),ylab=waiver(),name_file,cpt=NULL){
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
    filter(Variable%in%c(totvar, varlist), Year %in% byear) %>%
    filter(Name%in%bname,Model%in%bmodel,Scenario%in%bscen,category%in%bcat) %>% 
    pivot_wider(names_from = Variable, values_from = Value) %>% 
    mutate(across(c(starts_with(totvar)),~.x*100/eval(parse(text=totvar)))) %>% 
    select(-eval(totvar)) %>% 
    pivot_longer(cols=c(starts_with(totvar)),names_to="Variable",values_to="Value")
  w <- 250
  h <- ceiling(length(varlist)/3)*75
  if(nrow(dt)>0){
    p <- ggplot()+
      geom_boxplot(data=dt2, aes(x=Year, y=Value, group=Year), width=5)+
      geom_path(data=dt, aes(x=Year, y=Value, color=Scenario), show.legend=T, alpha=1, size=1)+
      labs(title=cpt, x=NULL, y=ylabel)+
      scale_y_continuous(limits=ylim, labels=ylab)
    if(length(var) > 1){
      p <- p+facet_wrap(~Variable,scales='free_y',ncol=3)
    }
    p <- p+theme_bw()+
      theme(legend.position='right', strip.background=element_blank(),panel.grid.minor=element_blank(),
            axis.text.x=element_text(angle=45, hjust=1))
    if(name_file!=F){
      ggsave(filename=paste0(odir,"/AR6/",name_file,".png"), plot=p, width=w, height=h, units='mm', dpi=300)
    }
    return(p)
  } else {
    print('error: empty data frame')
  }
}

fplot_jittershare <- function(var,totvar,scen,bname=unique(dfAR6$Name),bmodel=unique(dfAR6$Model),bscen=unique(dfAR6$Scenario),bcat=unique(dfAR6$category),byear,ylabel,ylim=c(0,NA),ylab=waiver(),name_file,cpt=NULL,labels=FALSE){
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
    filter(Variable%in%c(totvar, varlist), Year %in% byear) %>%
    filter(Name%in%bname,Model%in%bmodel,Scenario%in%bscen,category%in%bcat) %>% 
    pivot_wider(names_from = Variable, values_from = Value) %>% 
    mutate(across(c(starts_with(totvar)),~.x*100/eval(parse(text=totvar)))) %>% 
    select(-eval(totvar)) %>% 
    pivot_longer(cols=c(starts_with(totvar)),names_to="Variable",values_to="Value")
  w <- 250
  h <- ceiling(length(varlist)/3)*75
  if(nrow(dt)>0){
    p <- ggplot()+
      geom_jitter(data=dt2, aes(x=Year,y=Value,group=Year),width=0.5,size=1.5)+
      geom_path(data=dt, aes(x=Year, y=Value, color=Scenario), show.legend=T, alpha=1, size=1)+
      labs(title=cpt, x=NULL, y=ylabel)+
      scale_y_continuous(limits=ylim, labels=ylab)
    if(length(var) > 1){
      p <- p+facet_wrap(~Variable,scales='free_y',ncol=3)
    }
    if(labels){
      p <- p+geom_text(data=dt2,aes(x=Year,y=Value,label=Name),size=1.5,hjust=1.2,check_overlap=TRUE)
    }
    p <- p+theme_bw()+
      theme(legend.position='right', strip.background=element_blank(),panel.grid.minor=element_blank(),
            axis.text.x=element_text(angle=45, hjust=1))
    if(name_file!=F){
      ggsave(filename=paste0(odir,"/AR6/",name_file,".png"), plot=p, width=w, height=h, units='mm', dpi=300)
    }
    return(p)
  } else {
    print('error: empty data frame')
  }
}

fplot_lineshare <- function(var,totvar,scen,bname=unique(dfAR6$Name),bmodel=unique(dfAR6$Model),bscen=unique(dfAR6$Scenario),bcat=unique(dfAR6$category),ylabel,ylim=c(0,NA),ylab=waiver(),name_file,cpt=NULL){
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
    filter(Variable%in%c(totvar, varlist)) %>%
    filter(Name%in%bname,Model%in%bmodel,Scenario%in%bscen,category%in%bcat) %>% 
    pivot_wider(names_from = Variable, values_from = Value) %>% 
    mutate(across(c(starts_with(totvar)),~.x*100/eval(parse(text=totvar)))) %>% 
    select(-eval(totvar)) %>% 
    pivot_longer(cols=c(starts_with(totvar)),names_to="Variable",values_to="Value")
  w <- 250
  h <- ceiling(length(varlist)/3)*75
  if(nrow(dt)>0){
    p <- ggplot()+
      geom_line(data=dt2, aes(x=Year,y=Value,group=Name),size=0.5)+
      geom_path(data=dt, aes(x=Year, y=Value, color=Scenario), show.legend=T, alpha=1, size=0.5)+
      labs(title=cpt, x=NULL, y=ylabel)+
      scale_y_continuous(limits=ylim, labels=ylab)
    if(length(var) > 1){
      p <- p+facet_wrap(~Variable,scales='free_y',ncol=3)
    }
    p <- p+theme_bw()+
      theme(legend.position='right', strip.background=element_blank(),panel.grid.minor=element_blank(),
            axis.text.x=element_text(angle=45, hjust=1))
    if(name_file!=F){
      ggsave(filename=paste0(odir,"/AR6/",name_file,".png"), plot=p, width=w, height=h, units='mm', dpi=300)
    }
    return(p)
  } else {
    print('error: empty data frame')
  }
}

fplot_linepath <- function(var,scen,bname=unique(dfAR6$Name),bmodel=unique(dfAR6$Model),bscen=unique(dfAR6$Scenario),bcat=unique(dfAR6$category),byear,ylabel,ylim=NULL,ylab=waiver(),w=150,h=100,cpt=NULL,name_file,labels=FALSE){
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
      scale_y_continuous(limits=ylim, labels=ylab)
    if(length(var) > 1){
      p <- p+facet_wrap(~Variable,scales='free_y')
    }
    if(labels){
      p <- p+geom_text(data=dt2%>%filter(Year==2100),aes(x=Year,y=Value,label=Name),size=2.5,hjust=1.2,check_overlap=TRUE)
    }
    p <- p+theme_bw()+
      theme(legend.position='right', strip.background=element_blank(),panel.grid.minor=element_blank(),
            axis.text.x=element_text(angle=45, hjust=1))
    if(name_file!=F){
      ggsave(filename=paste0(odir,"/AR6/",name_file,".png"), plot=p, width=w, height=h, units='mm', dpi=300)
    }
    return(p)
  } else {
    print('error: empty data frame')
  }
}

fplot_bandspath <- function(var,scen,bname=unique(dfAR6$Name),bmodel=unique(dfAR6$Model),bscen=unique(dfAR6$Scenario),bcat=unique(dfAR6$category),
                             ylabel,ylim=NULL,ylab=waiver(),w=150,h=100,cpt=NULL,name_file,labels=FALSE,ylower=0.05,yupper=0.95){
  dt <- df_all %>% 
    filter(Variable %in% var) %>%
    mutate(Scenario=factor(Scenario, levels=scen_mat$Scenario)) %>%
    filter(Scenario!='historical') %>% 
    filter(Scenario%in%scen)
  dt2 <- dfAR6 %>% 
    filter(Variable%in%var) %>%
    filter(Name%in%bname,Model%in%bmodel,Scenario%in%bscen,category%in%bcat) %>% 
    filter(Yend==2100)
  dt2 <- func_fillY5(dt2) %>%
    mutate(Year=as.numeric(Year)) %>% 
    group_by(Year) %>% 
    mutate(mean=mean(Value),median=median(Value)) #%>% 
#    pivot_longer(cols=c(mean,median),names_to="Flag",values_to="Valuef")
  if(nrow(dt)>0){
    p <- ggplot()+
      geom_bands(data=dt2, aes(x=Year,y=Value),lower=ylower,upper=yupper, alpha=0.3)+
      geom_bands(data=dt2, aes(x=Year,y=Value),lower=0.25,upper=0.75, alpha=0.3)+
      geom_line(data=dt2, aes(x=Year,y=median),size=0.5, show.legend=F)+
      geom_line(data=dt2, aes(x=Year,y=mean),size=0.5, show.legend=F, linetype="dashed")+
      geom_path(data=dt, aes(x=Year, y=Value, color=Scenario), show.legend=T, alpha=1, size=0.5)+
      labs(title=cpt, x=NULL, y=ylabel, color="Scenario")+
      scale_y_continuous(limits=ylim, labels=ylab)
    if(length(var) > 1){
      p <- p+facet_wrap(~Variable,scales='free_y')
    }
    if(labels){
      p <- p+geom_text(data=dt2%>%filter(Year==2100),aes(x=Year,y=Value,label=Name),size=2.5,hjust=1.2,check_overlap=TRUE)
    }
    p <- p+theme_bw()+
      theme(legend.position='right', strip.background=element_blank(),panel.grid.minor=element_blank(),
            axis.text.x=element_text(angle=45, hjust=1))
    if(name_file!=F){
      ggsave(filename=paste0(odir,"/AR6/",name_file,".png"), plot=p, width=w, height=h, units='mm', dpi=300)
    }
    return(p)
  } else {
    print('error: empty data frame')
  }
}

