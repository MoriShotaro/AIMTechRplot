# Make output directory ---------------------------------------------------

if(!dir.exists(paste(odir,"index",sep="/"))){dir.create(paste(odir,"index",sep="/"),recursive=T)}
pdf(paste(odir,'index','plot_main.pdf',sep="/"),width=10,height=7)


# Function ----------------------------------------------------------------

fplot_share <- function(var,totvar,ylabel,ylim=c(0,NA),ylab=waiver(),name_file,cpt=NULL){
  varlist <- str_c(totvar,var,sep="_")
  dt <-  df_all %>% 
    filter(Variable %in% c(totvar,varlist)) %>% 
    pivot_wider(names_from = Variable, values_from = Value) %>%
    mutate(across(c(starts_with(totvar)),~.x*100/eval(parse(text=totvar)))) %>% 
    select(-eval(totvar)) %>% 
    pivot_longer(cols=c(starts_with(totvar)),names_to="Variable",values_to="Value") %>% 
    mutate(Scenario=factor(Scenario, levels=scen_mat$Scenario)) %>%
    filter(Scenario!='historical')
  w <- 250
  h <- ceiling(length(varlist)/3)*75
  if(nrow(dt)>0){
    p <- ggplot(data=dt)+
      geom_path(aes(x=Year, y=Value, color=Scenario), show.legend=T, alpha=1)+
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


# Plot --------------------------------------------------------------------

var_prm <- c("Oil","Coa","Gas","Solar","Win")
fplot_share(var_prm,"Prm_Ene",name_file="SHR_prm",ylabel="Share in primary energy (%)",cpt="SHR_prm")

var_fin <- c("Liq_Oil","SolidsCoa","Gas","Ele","Hyd")
fplot_share(var_fin,"Fin_Ene",name_file="SHR_fin",ylabel="Share in final energy (%)",cpt="SHR_fin")

var_finind <- c("Liq_Oil","SolidsCoa","Gas","Ele","Hyd")
fplot_share(var_finind,"Fin_Ene_Ind",name_file="SHR_finind",ylabel="Share in final energy (%)",cpt="SHR_finind")

var_finbui <- c("Liq_Oil","SolidsCoa","Gas","Ele","Hyd")
fplot_share(var_finbui,"Fin_Ene_Res_and_Com",name_file="SHR_finbui",ylabel="Share in final energy (%)",cpt="SHR_finbui")

var_fintra <- c("Liq_Oil","SolidsCoa","Gas","Ele","Hyd")
fplot_share(var_fintra,"Fin_Ene_Tra",name_file="SHR_fintra",ylabel="Share in final energy (%)",cpt="SHR_fintra")

dev.off()
