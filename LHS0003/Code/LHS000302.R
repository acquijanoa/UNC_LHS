##########################################
##
##      UNC LATIN AMERICAN HEALTH STUDIES
## 
## File: LHS000302.R
## 
## Project: Women obesity in Honduras
##
## Description: Dataset analysis using survey package
##
## Programmer: Álvaro Quijano
##
## Date: 09/24/2024
## 
############################################

## loading the data
load("../Data/LHS000301.Rdata")

## Uploading libraries
library(haven)
library(dplyr)
library(magrittr)
library(survey)
library(ggplot2)

## We create the design
hond_design = survey::svydesign(ids=~PSU, 
                                strata=~stratum,
                                weights=~wmweight, 
                                data = LHS000301)

### Creating a subset of observations
hond_design_sub = subset(hond_design, DOMAIN==1)


### modeling the overweight vs the age
mod_age = svyglm(formula = OVERWEIGHT ~ -1 + WOMEN_AGE_C4 , design = hond_design_sub, 
                 family = quasibinomial(link="log"))
mod_wealth = svyglm(formula = OVERWEIGHT ~ -1 + as.factor(windex5) , design = hond_design_sub, 
                    family = quasibinomial(link="log"))
mod_urban = svyglm(formula = OVERWEIGHT ~ -1 + factor(URBAN), design = hond_design_sub,
                   family = quasibinomial(link="log"))
mod_region = svyglm(formula = OVERWEIGHT ~-1 + REGION, design = hond_design_sub, 
                    family = quasibinomial(link="log"))
mod_education = svyglm(formula = OVERWEIGHT ~ -1 + factor(WOMEN_EDUCATION_C3), design = hond_design_sub, 
                       family = quasibinomial(link="log"))
mod_mstatus = svyglm(formula = OVERWEIGHT ~ -1 + factor(MSTATUS) + factor(WOMEN_AGE_C7), design = hond_design_sub, 
                     family = quasibinomial(link="logit"))
mod_region_windex = svyglm(formula = OVERWEIGHT ~ factor(REGION) +  
                                    factor(windex5), design = hond_design_sub, 
                     family = quasibinomial(link="logit"))

summary(mod_ethn)
### Obtaining the prevalences
exp(mod_age$coefficients)
exp(mod_wealth$coefficients)
exp(mod_urban$coefficients)
exp(mod_region$coefficients)
exp(mod_education$coefficients)
exp(mod_mstatus$coefficients)
exp(mod_ethn$coefficients)


### Creating the dataset with predicted values
db_pred = LHS000301 %>% filter(DOMAIN==1)
db_pred$prob_overweight =  predict(mod_region_windex, type="response")
db_pred %<>%  select(REGION,windex5, prob_overweight) %>% distinct()

save(db_pred, file = "LHS00302.Rdata")


#### 
db = data.frame(`Age` = LHS000301 %>% filter(DOMAIN==1) %>% pull(WOMEN_AGE_C7), exp(predict(mod_age))) %>% distinct()
db$Age = relevel(db$Age, levels=c("[15,20)","[20,25)","[25,30)","[30,35)","[35,40)","[40,45)","[45,50]"), ref= "[15,20)")

ggplot(db, aes(x=as.numeric(Age), y=link)) + 
  geom_point() + 
  geom_line() +
  theme_bw() + ylim(0,1) +
  labs(title="Overweight prevalence by age", y = "Prevalence", x = "Age")

### Model with all the variables
mod_education = svyglm(formula =  OVERWEIGHT ~ REGION + URBAN,  
                       design = hond_design_sub, 
                       family = quasibinomial(link="log"))
summary(mod_education)
exp(mod_education$coefficients)

pred.db = LHS000301 %>% filter(DOMAIN==1) %>% 
          mutate(pred = predict(mod_education, type = "response")) %>% 
          select(REGION,URBAN,pred) %>% distinct()
pred.db %>% View()





