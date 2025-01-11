####################################################################################
##
##                      UNC LATIN AMERICAN HEALTH STUDIES
## 
## File: LHS000502.R
## 
## Project: Obtain the predicted probability of overweight/obesity  by
##                - Wealth Index
##                - Women_Age_C4
##                - Women_Education_C4
##
## Description: 
##
## Programmer: Álvaro Quijano
##
## Date: 01/10/2025
## 
######################################################################################
# Upload library
library(dplyr)
library(survey)

## Load commandLine arguments
args <- commandArgs(trailingOnly = TRUE)
input_file <- args[1]
output_file <- args[2]
output_file2 <- args[3]
output_file3 <- args[4]

# Load Rdata that contains the derived dataset
load(input_file)

## attaching the variables names to the
attach(LHS000501)

## We set the sampling design
hond_design = survey::svydesign(ids=~PSU, 
                                strata=~stratum,
                                weights=~wmweight, 
                                data = LHS000501)

### Creating a subset of observations
hond_design_sub = subset(hond_design, DOMAIN==1)

### Run the model by wealth index
res.glm = survey::svyglm(OVERWEIGHT ~ WINDEX + REGION - 1, 
               family = quasibinomial(link="logit"), 
               design = hond_design_sub )

### Run the model by women age
res.glm2 = survey::svyglm(OVERWEIGHT~ WOMEN_AGE_C4 + REGION - 1, 
                         family = quasibinomial(link="logit"), 
                         design = hond_design_sub )

### Run the model by  women education
res.glm3 = survey::svyglm(OVERWEIGHT~ WOMEN_EDUCATION_C4 + REGION - 1, 
                          family = quasibinomial(link="logit"), 
                          design = hond_design_sub )

### Design-based predictions and std. errors of probabilities 
pred = predict(res.glm, type="response")
pred2 = predict(res.glm2, type="response")
pred3 = predict(res.glm3, type="response")

### Generate predictions
new.data = data.frame(pred, res.glm$data[,c('REGION','WINDEX')]  ) %>% 
              distinct() %>% 
              mutate(VAR = SE^2) %>% 
              select(REGION,WINDEX,response,VAR) %>% 
              rename(PROB = response) 

new.data2 = data.frame(pred2, res.glm$data[,c('REGION','WOMEN_AGE_C4')]  ) %>% 
              distinct() %>% 
              mutate(VAR = SE^2) %>% 
              select(REGION,WOMEN_AGE_C4,response,VAR) %>% 
              rename(PROB = response)

new.data3 = data.frame(pred3, res.glm$data[,c('REGION','WOMEN_EDUCATION_C4')]  ) %>% 
              distinct() %>% 
              mutate(VAR = SE^2)  %>% 
              select(REGION,WOMEN_EDUCATION_C4,response,VAR) %>% 
              rename(PROB = response)

### Save data
write.csv(new.data, output_file)
write.csv(new.data2, output_file2)
write.csv(new.data3, output_file3)
