MyThemeLine <- theme_bw() +
  theme(
    panel.border=element_blank(),
    panel.grid.minor = element_line(color = NA), 
    axis.line=element_line(colour="black"),
    panel.background=element_rect(fill = "white"),
    panel.grid.major=element_blank(),
    strip.background=element_rect(fill="white", colour="white"),
    strip.text.x = element_text(size=10, colour = "black", angle = 0,face="bold"),
    axis.text.x=element_text(size = 10,angle=45, vjust=0.9, hjust=1, margin = unit(c(t = 0.3, r = 0, b = 0, l = 0), "cm")),
    axis.text.y=element_text(size = 10,margin = unit(c(t = 0, r = 0.3, b = 0, l = 0), "cm")),
    legend.text = element_text(size = 10),
    axis.ticks.length=unit(0.15,"cm")
  )

fplot_area <- function(var, totvar=NA, ylabel, w=150, h=100, cpt=NULL, name_file, adj_position='stack'){
  dt <- filter(df_all, Variable %in% var$Variable) %>%
    mutate(Scenario=factor(Scenario, levels=scen_mat$Scenario)) %>%
    mutate(cat1=scen_mat$scen_cpol[Scenario], cat2=scen_mat$scen_techpol[Scenario]) %>%
    mutate(cat1=factor(cat1, levels=unique(scen_mat$scen_cpol))) %>%
    mutate(cat2=factor(cat2, levels=unique(scen_mat$scen_techpol))) %>%
    mutate(Variable=factor(Variable, levels=rev(var$Variable))) %>%
    filter(Scenario!='historical')
  dt_tot <- filter(df_all, Variable==totvar) %>%
    mutate(Scenario=factor(Scenario, levels=scen_mat$Scenario)) %>%
    mutate(cat1=scen_mat$scen_cpol[Scenario], cat2=scen_mat$scen_techpol[Scenario]) %>%
    mutate(cat1=factor(cat1, levels=unique(scen_mat$scen_cpol))) %>%
    mutate(cat2=factor(cat2, levels=unique(scen_mat$scen_techpol))) %>%
    filter(Scenario!='historical')
  plt$color <- as.character(var$Color); names(plt$color) <- as.character(var$Variable)
  plt$legend <- as.character(var$Legend); names(plt$legend) <- as.character(var$Variable)
  if(nrow(dt)>0){
    p <- ggplot(data=dt)+
      geom_area(aes(x=Year, y=Value, fill=Variable), position=adj_position, show.legend=T, alpha=1)+
      geom_path(data=dt_tot, aes(x=Year, y=Value), show.legend=F, alpha=1)+
      scale_fill_manual(values=plt$color, labels=plt$legend, name="Variable")
    if(all(is.na(scen_mat$scen_techpol))){
      p <- p + facet_grid(.~cat1)
    } else {
      p <- p + facet_grid(cat2~cat1)}
    p <- p+labs(title=cpt, x=NULL, y=ylabel)+
      theme_bw()+
      theme(legend.position='bottom', legend.title=element_blank(), 
            axis.text.x=element_text(angle=45, hjust=1),
            strip.background=element_blank(),
            panel.grid.minor=element_blank())+
      MyThemeLine
    if(name_file!=F){
      ggsave(filename=paste0(odir,"/general/",name_file,".png"), plot=p, width=w, height=h, units='mm', dpi=300)
    }
    return(p)
  } else {
    print('error: empty data frame')
  }
}

fplot_bar <- function(var, per=year_all, ylabel, w=150, h=100, cpt=NULL, name_file){
  dt <- filter(df_all, Variable%in%var, Year%in%per) %>%
    filter(!(Year==2010 & Scenario%in%scen_all[-1])) %>%
    mutate(Scenario=factor(Scenario, levels=scen_mat$Scenario)) %>%
    mutate(Variable=factor(Variable, levels=rev(var$Variable))) %>%
    filter(Scenario!='historical')
  plt$color <- as.character(var$Color); names(plt$color) <- as.character(var$Variable)
  plt$legend <- as.character(var$Legend); names(plt$legend) <- as.character(var$Variable)
  if(nrow(dt)>0){
    p <- ggplot(data=dt)+
      geom_bar(aes(x=Scenario, y=Value, fill=Variable), stat='identity', position='stack', show.legend=T, alpha=1)+
      scale_fill_manual(values=plt$color, labels=plt$legend, name='Variable')+
      facet_grid(.~Year, scales='free_x', space='free_x')+
      labs(title=cpt, x=NULL, y=ylabel)+
      theme_bw()+
      theme(legend.position='right', strip.background=element_blank(),panel.grid.minor=element_blank())+
      MyThemeLine
    if(name_file!=F){
      ggsave(filename=paste0(odir,"/plot/",reg_in,"/", name_file, ".png"), plot=p, width=w, height=h, units='mm', dpi=300)
    }
    return(p)
  } else {
    print('error: empty data frame')
  }
}

fplot_fbar <- function(var, ylabel, w=150, h=100, cpt=NULL, name_file, adj_position='fill'){
  dt <- filter(df_all, Variable %in% var$Variable) %>%
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
      p <- p + facet_grid(cat2~cat1)}
    p <- p+labs(title=cpt, x=NULL, y=ylabel)+
      theme_bw()+
      theme(legend.position='bottom', legend.title=element_blank(), 
            axis.text.x=element_text(angle=45, hjust=1),
            strip.background=element_blank(),
            panel.grid.minor=element_blank())+
      MyThemeLine
    if(name_file!=F){
      ggsave(filename=paste0(odir,"/general/",name_file,".png"), plot=p, width=w, height=h, units='mm', dpi=300)
    }
    return(p)
  } else {
    print('error: empty data frame')
  }
}

fplot_path <- function(var, ylabel, ylim=NULL, ylab=waiver(), w=150, h=100, cpt=NULL, name_file){
  dt <- filter(df_all, Variable %in% var) %>%
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
      ggsave(filename=paste0(odir,"/general/",name_file,".png"), plot=p, width=w, height=h, units='mm', dpi=300)
    }
    return(p)
  } else {
    print('error: empty data frame')
  }
}

fplot_scatter <- function(vars, xlab=NULL, ylab=NULL, w=150, h=100, cpt=NULL, name_file){
  dt <- filter(df_all, Variable%in%vars) %>%
    spread(key=Variable, value=Value, fill=NA) %>%
    rename(Value_x=vars[1], Value_y=vars[2]) %>%
    mutate(Scenario=factor(dt$Scenario, levels=scen_mat$Scenario), Year=as.character(Year)) %>%
    filter(Scenario!='historical')
  if(nrow(dt)>0){
    p <- ggplot(data=dt)+
      geom_point(aes(x=Value_x, y=Value_y, color=Year, shape=Scenario), show.legend=T, alpha=1)+
      labs(title=cpt, x=xlab, y=ylab)+
      theme_bw()+
      theme(legend.position='right', strip.background=element_blank(),panel.grid.minor=element_blank())+
      MyThemeLine
    if(name_file!=F){
      ggsave(filename=paste0(odir,"/plot/",reg_in,"/", name_file, ".png"), plot=p, width=w, height=h, units='mm', dpi=300)
    }
    return(p)
  } else {
    print('error: empty data frame')
  }
}

fplot_loadcurve <- function(dt_inp,reg,scen, ylabel, w=150, h=100, cpt=NULL, name_file){
  dt <- filter(dt_inp, Region %in% reg, Scenario %in% scen) %>%
    mutate(Year=as.character(Year))
  if(nrow(dt)>0){
    p <- ggplot(data=dt)+
      geom_path(aes(x=Time, y=Value, group=Year, color=Year), show.legend=T, alpha=1)+
      facet_grid(Season~Day)+
      labs(title=cpt, x=NULL, y=ylabel)+
      theme_bw()+
      theme(legend.position='right', strip.background=element_blank(),panel.grid.minor=element_blank())+
      MyThemeLine
    if(name_file!=F){
      ggsave(filename=paste0(odir,"/plot/",reg_in,"/", name_file, ".png"), plot=p, width=w, height=h, units='mm', dpi=300)
    }
    return(p)
  } else {
    print('error: empty data frame')
  }
}
