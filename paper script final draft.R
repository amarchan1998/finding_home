# Alejandra Marchan
# Universidad San Francisco de Quito
# Undergraduate Thesis
# Paper Script Final Draft


# Preliminaries -----------------------------------------------------------


# Load packages 

library(lmtest) # both for heteroskedasticty robust estimation
library(car) # For Wald testing
library(sandwich) # for better tables with robust errors
library(tidyverse) # ggplot and others
library(stargazer) # for latex and text exporting

# Load base

df<-read.csv('mig_reduced.csv')
df$imp<-as.factor(df$imp)
df1<-read.csv('without_mena_reduced.csv')
df2<-read.csv('occident_mig.csv')

# Table 1 Models ----------------------------------------------------------

# Model 1 Economic Indicators First Model (no log of population)

mdl_lei<-lm(ims~log(gdp_pc)+unem+timebuss, data=df)
summary(mdl_lei)

mdlh_lei<-coeftest(mdl_lei,vcov=hccm(mdl_lei,type='hc1'))
print(mdlh_lei)

# Model 2 Previous one with CCI and democracy

mdl_wgi<-lm(ims~log(gdp_pc)+unem+timebuss+cci+democracy,data=df)
summary(mdl_wgi)

mdlh_wgi<-coeftest(mdl_wgi,vcov=hccm(mdl_wgi,type='hc1')) 
print(mdlh_wgi)


# Model 3 With immigration policy dummy

df$imp<-relevel(df$imp,'Maintain') # Change the reference level

mdl_pol<-lm(ims~log(gdp_pc)+unem+timebuss+cci+democracy+imp,data=df)
summary(mdl_pol)

mdlh_pol<-coeftest(mdl_pol,vcov=hccm(mdl_pol,type='hc1'))
print(mdlh_pol)

# Model 4 with MENA dummy

mdl_reg<-lm(ims~log(gdp_pc)+unem+timebuss+cci+democracy+imp+mena,data=df)
summary(mdl_reg)

mdlh_reg<-coeftest(mdl_reg, vcov=hccm(mdl_reg,type='hc1'))
print(mdlh_reg)

### 5. As the one before but with the hief

mdl_hief<-lm(ims~log(gdp_pc)+unem+timebuss+cci+democracy+imp+mena+hief,data=df)
summary(mdl_hief)

mdlh_hief<-coeftest(mdl_hief, vcov=hccm(mdl_hief,type='hc1'))
print(mdlh_hief)


# Model 6 with public sector indicators

mdl_ps<-lm(ims~log(gdp_pc)+unem+timebuss+cci+democracy+imp+mena+hief+govexp,data=df)
summary(mdl_ps)

mdlh_ps<-coeftest(mdl_ps, vcov=hccm(mdl_ps,type='hc1'))
print(mdlh_ps)


# Stargazer for Table 1 ---------------------------------------------------

# Robust errors vectors for the stargazer tables

cov1<-vcovHC(mdl_lei,type='HC1')
robust1<-sqrt(diag(cov1))

cov2<-vcovHC(mdl_wgi,type='HC1')
robust2<-sqrt(diag(cov2))

cov3<-vcovHC(mdl_pol,type='HC1')
robust3<-sqrt(diag(cov3))

cov4<-vcovHC(mdl_reg,type='HC1')
robust4<-sqrt(diag(cov4))

cov5<-vcovHC(mdl_hief,type='HC1')
robust5<-sqrt(diag(cov5))

cov6<-vcovHC(mdl_ps,type='HC1')
robust6<-sqrt(diag(cov6))

# Covariate Labels Vector

x_names1<-c('Intercept',
            'Ln GDP per Capita', 
            'Unemployment',	
            'Days to Start a Business',
            'Control of Corruption',	
            'Democracy',	
            'Lower Mig. Policy ', 
            'Raise Mig. Policy ',	
            'MENA',	
            'Ethnic Diversity',
            'Government Exp. (% of GDP)')

# Stargazer Code for table 1

table1<-stargazer(mdl_lei,mdl_wgi, mdl_pol,mdl_reg,mdl_hief, mdl_ps,
          type='latex',
          title='Models considering economic, political and cultural indicators',
          covariate.labels=x_names1,
          dep.var.labels='International Migrant Stock (% of population)',
          se=list(robust1,robust2,robust3,robust4,robust5,robust6),
          omit.stat = c('rsq','f','ser'),
          align=F,
          dep.var.caption = '',
          no.space=T,
          intercept.bottom = F,
          intercept.top = T)


# Table 2 Models ----------------------------------------------------------

# Model 1: Using the interaction democracy*mena

mdl_int1<-lm(ims~democracy*mena+log(gdp_pc)+timebuss+cci+hief, data=df)
summary(mdl_int1)

mdlh_int1<-coeftest(mdl_int1, vcov=hccm(mdl_int1,type='hc1'))
print(mdlh_int1)


# Model 2:  Using the interactions democracy*mena and democracy*lngdppc

mdl_int2<-lm(ims~democracy*(mena+log(gdp_pc))+timebuss+cci+hief, data=df)
summary(mdl_int2)

mdlh_int2<-coeftest(mdl_int2, vcov=hccm(mdl_int2,type='hc1'))
print(mdlh_int2)

# Model 3: Switching around the interactions

mdl_int3<-lm(ims~democracy*(mena+log(gdp_pc))+(log(gdp_pc)*mena)+timebuss+cci+hief, data=df)
summary(mdl_int3)

mdlh_int3<-coeftest(mdl_int3, vcov=hccm(mdl_int3,type='hc1'))
print(mdlh_int3)

#  Model 4: Replace mena with richoil

mdl_roil<-lm(ims~democracy*(log(gdp_pc)+richoil)+(log(gdp_pc)*richoil)+timebuss+cci+hief, data=df)
summary(mdl_roil)

mdlh_roil<-coeftest(mdl_roil, vcov=hccm(mdl_roil,type='hc1'))
print(mdlh_roil)

# Model 5: Repeat model 3 (without the mena countries)

mdl_wmena<-lm(ims~democracy*log(gdp_pc)+timebuss+cci+hief, data=df1)
summary(mdl_wmena)

mdlh_wmena<-coeftest(mdl_wmena, vcov=hccm(mdl_wmena,type='hc1'))
print(mdlh_wmena)

# Model 6: Only the west
mdl_wmena1<-lm(ims~democracy*log(gdp_pc)+timebuss+cci+hief, data=df2)
summary(mdl_wmena1)

mdlh_wmena1<-coeftest(mdl_wmena1, vcov=hccm(mdl_wmena1,type='hc1'))
print(mdlh_wmena1)

# Stargazer for Table 2 ---------------------------------------------------

cov1a<-vcovHC(mdl_int1,type='HC1')
robust1a<-sqrt(diag(cov1a))

cov2a<-vcovHC(mdl_int2,type='HC1')
robust2a<-sqrt(diag(cov2a))

cov3a<-vcovHC(mdl_int3,type='HC1')
robust3a<-sqrt(diag(cov3a))

cov4a<-vcovHC(mdl_roil,type='HC1')
robust4a<-sqrt(diag(cov4a))

cov5a<-vcovHC(mdl_wmena,type='HC1')
robust5a<-sqrt(diag(cov5a))

cov6a<-vcovHC(mdl_wmena1,type='HC1')
robust6a<-sqrt(diag(cov6a))

# Covariate Labels Vector

x_names2<-c('Intercept',
            'Political Regime (Democracy Score)',
            'MENA',
            'Ln GDP per Capita', 
            'ROEC',
            'Days to Start a Business',
            'Control of Corruption',
            'Ethnic Diversity',
            'Democracy and MENA Int.',
            'Ln GDPPC and Democracy Int.',
            'Ln GDPPC and MENA Int.',
            'Democracy and ROEC Int.',
            'Ln GDPPC and ROEC Int.')

# Stargazer Code for table 2

table2<-stargazer(mdl_int1,mdl_int2, mdl_int3,mdl_roil,mdl_wmena, mdl_wmena1,
                  #type='text',
                  title='Models exploring the democracy coefficient',
                  covariate.labels=x_names2,
                  dep.var.labels='International Migrant Stock (% of population)',
                  se=list(robust1a,robust2a,robust3a,robust4a,robust5a,robust6a),
                  omit.stat = c('rsq','f','ser'),
                  align=T,
                  dep.var.caption = '',
                  no.space=T,
                  intercept.bottom = F,
                  intercept.top = T) 

# Other -------------------------------------------------------------------
# Final descriptive stats

# Make a reduced base for the stats

df_red<-select(df,ims,gdp_pc,unem,timebuss,cci,democracy,hief,govexp)



# Calculate descriptive stats with stargazer
x_namesdes<-c('International Migrant Stock (% of Population)',
            'Ln GDP per Capita (2017 PPP)', 
            'Unemployment',	
            'Days Required to Start a Business',
            'Control of Corruption',	
            'Democracy Score',	
            'Historical Index of Ethnic Fractionalization (2013)',
            'Government Expenditure (% of GDP)')


stargazer(df_red,title='Descriptive Statistics for the Database',
          type='latex',
          covariate.labels = x_namesdes)





