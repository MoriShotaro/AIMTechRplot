library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(gdxrrw)

# args <- commandArgs(trailingOnly=T)
# rootdir <- args[1]
# rootdir <- getwd()

output_dir <- paste(odir,"techflow",sep="/")
input_dir <- paste(cdir,"data",project,date,sep="/")
scen_name <- "Baseline"

if(!dir.exists(output_dir)){dir.create(output_dir,recursive=T)}

ALL_S <- rgdx.set(str_c(input_dir,'/gdx_primary/',scen_name,'.gdx'),symName='I') 
df_out <- rgdx.set(str_c(input_dir,'/gdx_primary/',scen_name,'.gdx'),symName='FL_ILJ') %>% rename(FR='L',TO='J')
df_in <- rgdx.set(str_c(input_dir,'/gdx_primary/',scen_name,'.gdx'),symName='FL_ILK') %>% rename(FR='K',TO='L')
df_int <- rgdx.set(str_c(input_dir,'/gdx_primary/',scen_name,'.gdx'),symName='JE_KE') %>% 
  mutate(J=as.character(J),K=as.character(K)) %>% filter(J!=K) %>% select(-INT) %>% rename(FR='J',TO='K')

f_flowplot <- function(SECTOR){
  df_out0 <- filter(df_out,I==SECTOR)
  df_in0 <- filter(df_in,I==SECTOR)
  df_int0 <- mutate(df_int,I=SECTOR) %>% 
    filter(FR%in%unique(df_out0$TO),TO%in%unique(df_in0$FR))
  df <- bind_rows(df_out,df_in,df_int0) %>% 
    filter(I==SECTOR) %>% 
    distinct(I,FR,TO) %>% 
    mutate(FR=as.character(FR),TO=as.character(TO))
  df_out2 <- gather(df_out,key='SET',value='NODE','FR','TO') %>% 
    mutate(SHAPE=if_else(SET=='FR','rectangle','oval')) %>% 
    mutate(COLOR=if_else(SET=='FR','LightCyan','MistyRose'))
  df_in2 <- gather(df_in,key='SET',value='NODE','FR','TO') %>% 
    mutate(SHAPE=if_else(SET=='TO','rectangle','oval')) %>% 
    mutate(COLOR=if_else(SET=='TO','LightCyan','MistyRose'))
  df_int2 <- gather(df_int0,key='SET',value='NODE','FR','TO') %>% 
    mutate(SHAPE=if_else(SET=='TO','rectangle','oval')) %>% 
    mutate(COLOR=if_else(SET=='TO','LightCyan','MistyRose'))
  df2 <- bind_rows(df_out2,df_in2,df_int2) %>% 
    filter(I==SECTOR) %>% 
    distinct(NODE,.keep_all=T)
  
  #ls_node <- unique(c(df$FR,df$TO))
  ls_node <- df2$NODE
  shp_node <- df2$SHAPE
  col_node <- df2$COLOR
  
  nodes <- create_node_df(n=length(ls_node),type=ls_node,label=ls_node,shape=shp_node,fillcolor=col_node,
                          width=2.2,fontsize=25)
  edges <- create_edge_df(from=factor(df$FR,levels=ls_node),to=factor(df$TO,levels=ls_node),
                          arrowsize=1)
  
  create_graph(nodes_df=nodes,edges_df=edges) %>% 
    # set flow direction LR or TB
    add_global_graph_attrs('rankdir','LR','graph') %>% 
    add_global_graph_attrs('layout','dot','graph') %>% 
    add_global_graph_attrs('splines','ortho','graph') %>% 
    #    render_graph()
    DiagrammeR::export_graph(file_name=str_c(output_dir,"/",SECTOR,'.png'),file_type='PNG')
}
# ls_plot_sec <- c('PSS','FRG','H_H','CCS')
# lapply(ls_plot_sec,f_flowplot)

lapply(c('COL','OIL','GAS','H_H','BIM','CCS'),f_flowplot)
lapply(c('PSS','FRG','BNK'),f_flowplot)
