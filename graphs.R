# Alejandra Marchan
# Universidad San Francisco de Quito
# Undergraduate Thesis
# Paper Script Draft 1 Graphs

# Load packages 

library(tidyverse)
library(corrplot) # for correlograms
library(maps) # for chloropleth graphs
windowsFonts(TNR = windowsFont("Times New Roman")) # set times new roman font

 # For colors
library(RColorBrewer)
mycolors1<-brewer.pal(9, 'BuGn') 
mycolors1<- colorRampPalette(mycolors1)(30)

# Load database

mig_df<-read.csv('cross_section_mig.csv', fileEncoding = 'UTF-8-BOM')

# For policy without NA graph

hief_inmp<-read.csv('for_graph_hief_inmp.csv')

# Boxplots

# HIEF by policy type

hief_pol<-ggplot(hief_inmp, aes(x=inmp_cat, y=hief_2013))+geom_boxplot(color='black', fill='blue', alpha=0.5)
hief_pol+theme(axis.text.x =element_text(angle=30, hjust=1,vjust=1))+ 
  (labs(y="Historical Index of Ethnic Fractionalization (2013)",x = "Immigration Policy Type"))+
  theme_bw()+stat_summary(fun='mean',geom='point', shape=18,size=4, fill='white')+
  theme(text=element_text(size=12, family='TNR'), 
        axis.title.x=element_text(face='bold'),
        axis.title.y=element_text(face='bold'), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),)

# Refugee population by region

ref_reg<-ggplot(mig_df, aes(x=region, y=lgrefug_2015))+geom_boxplot(color='black', fill='red', alpha=0.4)
ref_reg+(labs(y="Ln Refugee Population",x = "Region"))+
  theme_bw()+stat_summary(fun='mean',geom='point', shape=18,size=4, fill='white')+
  theme(text=element_text(size=12, family='TNR'), 
        axis.title.x=element_text(face='bold'),
        axis.title.y=element_text(face='bold'), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x =element_text(angle=30, hjust=1,vjust=1))
  

# Refugee Population as a % of IMS by region
refp_reg<-ggplot(mig_df, aes(x=region, y=refug_perc))+geom_boxplot()
refp_reg+theme(axis.text.x =element_text(angle=30, hjust=1,vjust=1))+coord_cartesian(ylim=c(0,100))

# Scatter plots

# Income per capita and democracy, separating presence in the NAME region

# Make a new categorical variable which simply replaces 1 in africame with "NAME": 
mig_df$name_presence<-ifelse(mig_df$africame=='1','In MENA','Not in MENA')

gdp_dem_name<-ggplot(mig_df,aes(x=lngdppc_2015,y=democracy_2015, colour=name_presence))
gdp_dem_name +geom_point()+stat_smooth(method=lm, se=F)+theme_bw()+
  xlab('Ln GDP per Capita (2017 PPP Dollars)')+
  ylab('Democracy')+guides(colour=guide_legend(title=NULL))+
  theme(legend.position='bottom', text=element_text(size=12, family='TNR'), 
        axis.title.x=element_text(face='bold'),
        axis.title.y=element_text(face='bold'),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())  

# Relate Democracy with oil rents

dem_oil<-ggplot(mig_df, aes(x=oilrent_2015, y=democracy_2015, colour=name_presence))
dem_oil+geom_point()+theme_bw()+
  xlab('Oil Rents (% of GDP)')+ylab('Democracy')+guides(colour=guide_legend(title=NULL))+
  theme(text=element_text(size=12, family='TNR'), 
axis.title.x=element_text(face='bold'),
axis.title.y=element_text(face='bold'), 
panel.grid.major = element_blank(), 
panel.grid.minor = element_blank(),
legend.position='bottom')

# Democracy and IMS, separating by NAME or not in NAME. 

dem_ims_name<-ggplot(mig_df,aes(x=democracy_2015,y=ims_2015, colour=name_presence))
gdp_dem_name +geom_point()+stat_smooth(method=lm, se=F)+theme_bw()+
  xlab('Democracy')+
  ylab('International Migrant Stock')+guides(colour=guide_legend(title=NULL))+
  theme(legend.position='bottom', text=element_text(size=12, family='TNR'), 
        axis.title.x=element_text(face='bold'),
        axis.title.y=element_text(face='bold'),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())  

gdp_ims_name<-ggplot(mig_df,aes(x=lngdppc_2015,y=ims_2015, colour=name_presence))
gdp_dem_name +geom_point()+stat_smooth(method=lm, se=F)+theme_bw()+
  xlab('Ln of GDP per Capita (2017 PPP)')+
  ylab('International Migrant Stock')+guides(colour=guide_legend(title=NULL))+
  theme(legend.position='bottom', text=element_text(size=12, family='TNR'), 
        axis.title.x=element_text(face='bold'),
        axis.title.y=element_text(face='bold'),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# PSA and others

dem_psa_name<-ggplot(mig_df,aes(x=democracy_2015,y=psa_2015, colour=name_presence))
dem_psa_name +geom_point()+stat_smooth(method=lm, se=F)+theme_bw()+
  xlab('Democracy')+
  ylab('Political Stability and Absence of Violence')+guides(colour=guide_legend(title=NULL))+
  theme(legend.position='bottom', text=element_text(size=11, family='TNR'), 
        axis.title.x=element_text(face='bold'),
        axis.title.y=element_text(face='bold'),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())  

refug_psa_name<-ggplot(mig_df,aes(x=lgrefug_2015,y=psa_2015, colour=name_presence))
refug_psa_name +geom_point()+stat_smooth(method=lm, se=F)+theme_bw()+
  xlab('Ln Refugee Population')+
  ylab('Political Stability and Absence of Violence')+guides(colour=guide_legend(title=NULL))+
  theme(legend.position='bottom', text=element_text(size=11, family='TNR'), 
        axis.title.x=element_text(face='bold'),
        axis.title.y=element_text(face='bold'),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())  

gdp_psa_name<-ggplot(mig_df,aes(x=lngdppc_2015,y=psa_2015, colour=name_presence))
gdp_psa_name +geom_point()+stat_smooth(method=lm, se=F)+theme_bw()+
  xlab('Ln GDP Per Capita (2017 PPP)')+
  ylab('Political Stability and Absence of Violence')+guides(colour=guide_legend(title=NULL))+
  theme(legend.position='bottom', text=element_text(size=11, family='TNR'), 
        axis.title.x=element_text(face='bold'),
        axis.title.y=element_text(face='bold'),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 



psa_ims_name<-ggplot(mig_df,aes(x=psa_2015,y=ims_2015, colour=name_presence))
psa_ims_name +geom_point()+stat_smooth(method=lm, se=F)+theme_bw()+
  xlab('Political Stability and Absence of Violence')+
  ylab('International Migrant Stock')+guides(colour=guide_legend(title=NULL))+
  theme(legend.position='bottom', text=element_text(size=12, family='TNR'), 
        axis.title.x=element_text(face='bold'),
        axis.title.y=element_text(face='bold'),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())  

# Choropleth Maps

# Import the map coordinates along with the variables
map_data<-read.csv('mapa.csv')


# Map for International Migrant Stock ------------------------------------

## Create quartiles
qims <- quantile(map_data$ims_2015, c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6,
                                      0.7, 0.8, 0.9, 1.0), na.rm = TRUE)
qims


## Add a column of the quantile category
map_data$ims_2015_q <- cut(map_data$ims_2015, qims,
                        labels=c("0-10%", "10-20%", "20-30%", "30-40%", "40-50%", 
                                 "50-60%", "60-70%", "70-80%", "80-90%", "90-100%"),
                        include.lowest=TRUE)

## Generate a discrete color palette with 10 values
pal1 <- colorRampPalette(c('#d9f1ff', '#7EF9FF', '#1e2d47'))(10)
pal1

ims_map<- ggplot(map_data, aes(x=long,y=lat,group=group,fill=ims_2015_q)) 
ims_map+ geom_polygon(colour='black') + theme_void()+
  scale_fill_manual(values=pal1, na.value = '#575959') +theme(legend.position='bottom', 
    text=element_text(size=12, family='TNR'),
    axis.text.y = element_blank(),
    axis.text.x= element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())+
  labs(fill='International \nMigrant Stock') +
  xlab(NULL) +
  ylab(NULL)

  
###ims_p<-ggplot(map_data,aes(x=long,y=lat,group=group,fill=ims_2015))
###ims_p+geom_polygon(colour='black')+  theme_void()+
  #scale_fill_gradient2(low='#d9f1ff', mid='#7EF9FF', high='#1e2d47')+
  #theme(legend.position='bottom', text=element_text(size=12, family='TNR'))+
  #labs(fill='International \nMigrant Stock')


# Map for Democracy -------------------------------------------------------
## Create quartiles
qdem <- quantile(map_data$dem_2015, c(0, 0.15, 0.30, 0.45, 0.60, 0.80), na.rm = TRUE)
qdem
  
  
## Add a column of the quantile category
map_data$dem_2015_q <- cut(map_data$dem_2015, qdem,
                    labels=c( "-10;-1", "-1;4", "4;8", "8;9", "10"),
                    include.lowest=TRUE)
  
## Generate a discrete color palette with 10 values
pal2 <- colorRampPalette(c(low='#ACDF87', mid='#A4DE02', high='#1E5631'))(5)
pal2
  
dem_map<-ggplot(map_data, aes(x=long,y=lat,group=group,fill=dem_2015_q)) 
dem_map+geom_polygon(colour='black') +scale_fill_manual(values=pal2, na.value = '#575959')+
       theme_void()+
       theme(legend.position='bottom',
             text=element_text(size=12, family='TNR'),
             axis.text.y = element_blank(),
             axis.text.x= element_blank(),
             axis.ticks = element_blank(),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank() )+
       labs(fill='Democracy')+
       xlab(NULL)+
       ylab(NULL)
        
     
  
###dem_map<-ggplot(map_data,aes(x=long,y=lat,group=group,fill=dem_2015))
###dem_map+geom_polygon(colour='black')+  theme_void()+
 #scale_fill_gradient2(low='#ACDF87', mid='#A4DE02', high='#1E5631')+
  #theme(legend.position='bottom', text=element_text(size=12, family='TNR'))+
  #labs(fill='Democracy')

# Map for Historical Index of Ethnic Fractionalization --------------------

## Create quartiles
qhief <- quantile(map_data$hief_2013, c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6,
                                       0.7, 0.8, 0.9, 1.0), na.rm = TRUE)
qhief
 
## Add a column of the quantile category
map_data$hief_2013_q <- cut(map_data$hief_2013, qhief,
                            labels=c("0-10%", "10-20%", "20-30%", "30-40%", "40-50%", 
                                     "50-60%", "60-70%", "70-80%", "80-90%", "90-100%"),
                            include.lowest=TRUE)
 
## Generate a discrete color palette with 10 values
pal3 <- colorRampPalette(c(low='#d8cbf7', mid='#B99AFF', high='#2E165B'))(10)
pal3
 
hief_map<- ggplot(map_data, aes(x=long,y=lat,group=group,fill=hief_2013_q)) 
hief_map+ geom_polygon(colour='black') + theme_void()+
   scale_fill_manual(values=pal3, na.value ='#575959') +theme(legend.position='bottom', 
                     text=element_text(size=12, family='TNR'),
                     axis.text.y = element_blank(),
                     axis.text.x= element_blank(),
                     axis.ticks = element_blank(),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank())+
   labs(fill='Historical Index of \nEthnic Fractionalization')+
  xlab(NULL)+
  ylab(NULL)
 
 
#hief_map<-ggplot(map_data,aes(x=long,y=lat,group=group,fill=hief_2013))
#hief_map+geom_polygon(colour='black')+  theme_void()+
  #scale_fill_gradient2(low='#d8cbf7', mid='#B99AFF', high='#2E165B')+
  #theme(legend.position='bottom', text=element_text(size=12, family='TNR'))+
  #labs(fill='Historical Index of \nEthnic Fractionalization')


# Partial Effects Graphs

# First create the variables
df2$lgdp_pc<-log(df2$gdp_pc)
df2$pe_dem<-((df2$lgdp_pc)*0.823042)-8.437483
df2$pe_lgdp<-(df2$democracy*0.823042)-3.620

# Scatter plot of the effect of democracy in ims, show for values of LGDP
ape_dem<-ggplot(df2,aes(x=lgdp_pc,y=pe_dem))+geom_line(colour='#bad1de')+
         geom_point(colour='#050587')+theme_bw()
    ape_dem+xlab('Ln GDP per Capita (2017 PPP)')+
    ylab('Partial Effect of a One Point Democracy Increase in IMS')+
    theme(legend.position='bottom', 
        text=element_text(size=12, family='TNR'), 
        axis.title.x=element_text(face='bold'),
        axis.title.y=element_text(face='bold'),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
    annotate('label',x=11.05,y=-0.2, 
              label='Positive PE of Democracy \nat 58th GDPPC Percentile \n 28,327.34 (2017 PPP dollars) ',
              family='TNR')+
    geom_segment(x=0,y=0,xend=10.2515,yend=0,linetype='dashed', color='#ED2939')+
    geom_segment(x=10.2515,y=-2.2,xend=10.2515,yend=0, linetype='dashed',color='#ED2939')+

# Scatter plot of the effect of lgdp in ims, show for values of democracy
ape_lgdp<-ggplot(df2,aes(x=democracy,y=pe_lgdp))+geom_line(colour='#bad1de')+
          geom_point(colour='#050587')+theme_bw()
ape_lgdp+xlab('Democracy Score')+
      ylab('Partial Effect of a 1% GDPPC Increase in IMS')+
      theme(legend.position='bottom', 
            text=element_text(size=12, family='TNR'), 
            axis.title.x=element_text(face='bold'),
            axis.title.y=element_text(face='bold'),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())+
      annotate('label',x=7.7,y=-1, 
               label='Positive PE of GDDPC \nfor Democracy Scores over \n9th percentile (Anocracies)',
               family='TNR')+
      geom_segment(x=-8,y=0,xend=4.3982,yend=0,linetype='dashed', color='#ED2939')+
      geom_segment(x=4.3982,y=-10,xend=4.3982,yend=0, linetype='dashed',color='#ED2939')+
      geom_point(x=4.3982,y=0, color='#ED2939')
