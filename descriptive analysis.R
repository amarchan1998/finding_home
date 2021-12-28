# Alejandra Marchan
# Universidad San Francisco de Quito
# Undergraduate Thesis
# Paper Script Draft 1

# Load packages 

library(lmtest) # both for heteroskedasticty robust estimation
library(car)
library(sandwich) # for better tables with robust errors
library(tidyverse) # ggplot and others
library(broom) # tidy command for exporting
library(openxlsx) # for Excel exporting
library(stargazer) # for latex and text exporting
library(psych) # for descriptive statistics

# Load database

mig_df<-read.csv('cross_section_mig.csv', fileEncoding = 'UTF-8-BOM')

#---------- Estimate descriptive statistics and export them ----------------

stats<-describe(mig_df)
stats$varcoef<-stats$sd/stats$mean # variation coefficient
write.xlsx(stats,'descriptive_stats.xlsx', row.names=T)

#----- Create variables
med_dem<-median(mig_df$democracy_2015, na.rm = T)
mig_df$dem_dum<-ifelse(mig_df$democracy_2015>med_dem, 1,0)

mig_df$dem_sq<-mig_df$democracy_2015*mig_df$democracy_2015

# --------- Descriptive Stats by group----------
# Descriptive statistics by policy
ims_bypol<-describeBy(mig_df$ims_2015,group=mig_df$inmp_cat)
ims_bypol

hief_bypol<-describeBy(mig_df$hief_2013,group=mig_df$inmp_cat)
hief_bypol

# Descriptive stats by region
refug_byreg<-describeBy(mig_df$refugpop15, group=mig_df$region)
refug_byreg

mig_df$refug_perc<-(mig_df$refugpop15/mig_df$imst_2015)*100 # Create a Refugeee variable as % of IMS

rfgp_byreg<-describeBy(mig_df$refug_perc, group=mig_df$region)
rfgp_byreg

