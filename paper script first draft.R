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

# From now on we estimate models following the text flow in the first paper draft

# Results for Table 1 ------------------------------------------------------

### 1. LEI model

mdl_lei<-lm(ims_2015~lngdppc_2015+unem_2015+timebuss_2015+lgpop_2015, data=mig_df)
summary(mdl_lei)
mdlh_lei<-coeftest(mdl_lei,vcov=hccm(mdl_lei,type='hc1'))
print(mdlh_lei)

### 2. WGI plus democracy

mdl_wgi<-lm(ims_2015~lngdppc_2015+unem_2015+timebuss_2015+lgpop_2015+cci_2015
            +democracy_2015,data=mig_df)
summary(mdl_wgi)
mdlh_wgi<-coeftest(mdl_wgi,vcov=hccm(mdl_wgi,type='hc1')) 
print(mdlh_wgi)

### 3. With immigration policies
mdl_pol<-lm(ims_2015~lngdppc_2015+unem_2015+timebuss_2015+lgpop_2015+cci_2015+democracy_2015+inmp_raise15
             +inmp_lower15,data=mig_df)
summary(mdl_pol)
mdlh_pol<-coeftest(mdl_pol,vcov=hccm(mdl_pol,type='hc1'))
print(mdlh_pol)

### 4.With the best region 
mdl_reg<-lm(ims_2015~lngdppc_2015+unem_2015+timebuss_2015+lgpop_2015+cci_2015+democracy_2015+inmp_raise15
  +inmp_lower15+africame,data=mig_df)
summary(mdl_reg)
mdlh_reg<-coeftest(mdl_reg, vcov=hccm(mdl_reg,type='hc1'))
print(mdlh_reg)

### 5. With public sector

mdl_ps<-lm(ims_2015~lngdppc_2015+unem_2015+timebuss_2015+lgpop_2015+cci_2015+
             democracy_2015+inmp_raise15+inmp_lower15+africame+
             govfcexp_2015+geduc_2015+currenthexp_2015,data=mig_df)
summary(mdl_ps)
mdlh_ps<-coeftest(mdl_ps, vcov=hccm(mdl_ps,type='hc1'))
print(mdlh_ps)

### 6. Model with hief and inmp raise

mdl_hief<-lm(ims_2015~lngdppc_2015+unem_2015+timebuss_2015+lgpop_2015
             +cci_2015+democracy_2015+inmp_raise15+africame+hief_2013,data=mig_df)
summary(mdl_hief)
mdlh_hief<-coeftest(mdl_hief, vcov=hccm(mdl_hief,type='hc1'))
print(mdlh_hief)

### 7. Hief without policy and unemployment
mdl_hief1<-lm(ims_2015~lngdppc_2015+timebuss_2015+lgpop_2015
              +cci_2015+democracy_2015+africame+hief_2013,data=mig_df)
summary(mdl_hief1)
mdlh_hief1<-coeftest(mdl_hief1, vcov=hccm(mdl_hief1,type='hc1'))
print(mdlh_hief1)

#---------- Export the first table to Latex ----------

# Create model names and covariates too
covariate_namest1<-c('Intercept','Ln GDP per Capita', 'Unemployment',	
                   'Days Required to Start a Business',	
                   'Ln Population.',	'Control of Corruption',	
                   'Democracy',	'Raise Migration Dummy', 'Lower Migration Dummy',	
                   'North Africa and Middle East',	
                   'Government Current Expenditure (% of GDP)',
                   'Government Expenditure in Education (% of GDP)',
                  'Current Health Expenditure (% of GDP)', 
                  'Historical Index of Ethnic Fractionalization (2013')

# Matrix of robust errors for the 5 models

cov1<-hccm(mdl_lei,type='hc1')
robust1<-sqrt(diag(cov1))

cov2<-vcovHC(mdl_wgi,type='HC1')
robust2<-sqrt(diag(cov2))

cov3<-vcovHC(mdl_pol,type='HC1')
robust3<-sqrt(diag(cov3))

cov4<-vcovHC(mdl_reg,type='HC1')
robust4<-sqrt(diag(cov4))

cov5<-vcovHC(mdl_ps,type='HC1')
robust5<-sqrt(diag(cov5))

cov6<-vcovHC(mdl_hief,type='HC1')
robust6<-sqrt(diag(cov6))

cov7<-vcovHC(mdl_hief1,type='HC1')
robust7<-sqrt(diag(cov7))


latex_t1<-stargazer(mdl_lei,mdl_wgi, mdl_pol,mdl_reg,mdl_ps, mdl_hief, mdl_hief1, 
                    title='Introductory Models',
                    covariate.labels = covariate_namest1,
                    dep.var.labels = 'International Migrant Stock',
                    se=list(robust1,robust2,rbust3,robust4,robust5,robust6, robust7), type='text', 
                    omit.stat = c('rsq','f','ser'), align=T, 
                    dep.var.caption = "", no.space=T, intercept.bottom = F, intercept.top = T)

# Results for Table 2 Exploring Democracy --------------------------------

# 1. Using the interaction democracy.africame

mdl_int1<-lm(ims_2015~lngdppc_2015+timebuss_2015+cci_2015+hief_2013+lgpop_2015+
               democracy_2015*africame, data=mig_df)
summary(mdl_int1)
mdlh_int1<-coeftest(mdl_int1, vcov=hccm(mdl_int1,type='hc1'))
print(mdlh_int1)

# 2. Using the interactions democracy.africame and democracy.lngdppc

mdl_int2<-lm(ims_2015~lngdppc_2015+timebuss_2015+cci_2015+hief_2013+lgpop_2015+
               democracy_2015*africame+democracy_2015*lngdppc_2015
             +africame+democracy_2015, data=mig_df)
summary(mdl_int2)
mdlh_int2<-coeftest(mdl_int2, vcov=hccm(mdl_int2,type='hc1'))
print(mdlh_int2)


# 3. When controlling for refugee population (log)
mdl_rfg1<-lm(ims_2015~lngdppc_2015+timebuss_2015+cci_2015+hief_2013+lgpop_2015+
               +democracy_2015*lngdppc_2015+africame+democracy_2015+lgrefug_2015
             , data=mig_df)
summary(mdl_rfg1)
mdlh_rfg1<-coeftest(mdl_rfg1, vcov=hccm(mdl_rfg1,type='hc1'))
print(mdlh_rfg1)

# 4. Rich oil and some interactions

mdl_oil1<-lm(ims_2015~lngdppc_2015+cci_2015+democracy_2015+
              democracy_2015*lngdppc_2015+richoil_2015+hief_2013+
              lgrefug_2015+lgpop_2015+richoil_2015*democracy_2015
            + cci_2015*lngdppc_2015+cci_2015*hief_2013+hief_2013*richoil_2015+
              timebuss_2015+timebuss_2015*cci_2015+richoil_2015*lngdppc_2015
            , data=mig_df)
summary(mdl_oil1)
mdlh_oil1<-coeftest(mdl_oil1, vcov=hccm(mdl_oil1,type='hc1'))
print(mdlh_oil1)

# 5. Use democracy dummy and richoil dummy


mdl_oildum<-lm(ims_2015~lngdppc_2015+cci_2015+dem_dum+
               dem_dum*lngdppc_2015+richoil_2015+hief_2013+
               lgrefug_2015+lgpop_2015+richoil_2015*dem_dum
             + cci_2015*lngdppc_2015+cci_2015*hief_2013+hief_2013*richoil_2015+
               timebuss_2015+timebuss_2015*cci_2015+richoil_2015*lngdppc_2015
             , data=mig_df)
summary(mdl_oildum)
mdlh_oildum<-coeftest(mdl_oildum, vcov=hccm(mdl_oildum,type='hc1'))
print(mdlh_oildum)

# 6. Remove Population

mdl_oil1p<-lm(ims_2015~lngdppc_2015+cci_2015+democracy_2015+
               democracy_2015*lngdppc_2015+richoil_2015+hief_2013+
               lgrefug_2015+richoil_2015*democracy_2015
             + cci_2015*lngdppc_2015+cci_2015*hief_2013+hief_2013*richoil_2015+
               timebuss_2015+timebuss_2015*cci_2015+richoil_2015*lngdppc_2015
             , data=mig_df)
summary(mdl_oil1p)
mdlh_oil1p<-coeftest(mdl_oil1p, vcov=hccm(mdl_oil1p,type='hc1'))
print(mdlh_oil1p)


#--- Export Table 2 Results to Latex---

# First compute robust errors

cov1b<-vcovHC(mdl_int1,type='HC1')
robust1b<-sqrt(diag(cov1b))

cov2b<-vcovHC(mdl_int2,type='HC1')
robust2b<-sqrt(diag(cov2b))

cov3b<-vcovHC(mdl_rfg1,type='HC1')
robust3b<-sqrt(diag(cov3b))

cov4b<-vcovHC(mdl_oil1,type='HC1')
robust4b<-sqrt(diag(cov4b))

cov5b<-vcovHC(mdl_oildum,type='HC1')
robust5b<-sqrt(diag(cov5b))

cov6b<-vcovHC(mdl_oil1,type='HC1')
robust6b<-sqrt(diag(cov6b))

cov7b<-vcovHC(reg2,type='HC1')
robust7b<-sqrt(diag(cov7b))


covariate_namest2<-c('Intercept','Ln GDPPC','Days Required to Start a Business',
                     'Interaction Ln GDPPC and Democracy Dummy', 
                     'Interaction Rich Oil Exporters and Democracy Dummy',
                     'Control of Corruption',
                     'Democracy Dummy',
                     'Historical Index of Ethnic Fractionalization (2013)', 
                     'Ln Population',
                     'Interaction Democracy and Ln GDPPC',
                     'Democracy',
                     'Middle East and North Africa',
                     'Interaction MENA and Democracy',
                     'Rich Oil Exporters', 
                     'Ln Refugee Population',
                     'Interaction Ln GDPPC and Democracy',
                     'Interaction Democracy and Rich Oil Exporters', 
                     'Interaction Corruption and Ln GDPPC',
                     'Interaction Corruption and Ethnic Fractionalization',
                     'Interaction Rich Oil Exporters and Ethnic Fractionalization',
                     'Interaction Days Required and Corruption',
                     'Interaction Ln GDPPC and Rich Oil Exporers'
                     )

latex_table2<-stargazer(mdl_int1,mdl_int2,mdl_rfg1,mdl_oil1,mdl_oildum, mdl_oil1p,reg2,
                        title='Exploring the relationship of democracy and migration',
                        covariate.labels = covariate_namest2,
                        dep.var.labels = 'International Migrant Stock',
                        se=list(robust1b,robust2b, robust3b,robust4b,robust5b, robust6b, robust7b), 
                        type='text', 
                        omit.stat = c('rsq','f','ser'), align=T, 
                        dep.var.caption = "", no.space=T, intercept.bottom = F, 
                        intercept.top = T)


