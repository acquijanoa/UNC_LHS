###########################################################
# Project Name   : LHS0002051
# Script Name    : LHS0002051.R
# Author         : Álvaro Quijano
# Date Created   : 08/24/24
# Last Modified  : 08/24/24
# Version        : R version 4.3.3
# Description    : This estimates the MDD prevalence by region
#
# Inputs         : 
#   - [Input File 1: Brief description]
#   - [Input File 2: Brief description]
#
# Outputs        : 
#   - [Output File 1: Brief description]
#   - [Output File 2: Brief description]
#
# Dependencies   : 
#   - [Dependency 1: Brief description or link]
#   - [Dependency 2: Brief description or link]
#
# Usage Notes    : 
#   - [Any important usage notes]
#
# Revision History:
#   - [YYYY-MM-DD]: [Description of changes made]
#   - [YYYY-MM-DD]: [Description of changes made]
###########################################################
library(haven)
library(survey)
library(dplyr)
setwd("~/Documentos/Research/LATAM_HS/LHS0002")

## Reading the data
data = read_sas("data/lhs000201.sas7bdat")
data_ = data %>% filter(in_6_23m == 1)
attach(data)


## Creating the design
desgn.mod = survey::svydesign(data = data,
                              ids =~PSU, 
                              strata =~stratum,  
                              weights =~WT, 
                              check.strata = T)
desgn.sub = subset(desgn.mod, in_6_23m == 1)

### Design-based estimates by department
mod = survey::svyglm(MDD~ factor(DEPARTMENT)+factor(URBAN),  
                     family = quasibinomial(link="log"), design = desgn.sub)
summary(mod)
exp(mod$coefficients)

data_$pred = exp(predict(mod, type = "link"))
data_$pred.se = attr(predict(mod, type = "response"),"var")

data_ %>% select(DEPARTMENT,URBAN,pred,pred.se) %>% 
          unique() %>% arrange(DEPARTMENT) %>% View()


{
t = seq(0,5, 0.01)
plot(t, exp(-(1/3)*t^(3/2) ), 
          type="l", col="red", xlab ="time", ylab="Survival")
lines(t, exp(-(2/3)*t^(3/2) ), col = "blue")
lines(t, exp(-t), col = "green")
legend(legend = c("CVD treatment","CVD placebo","Non-CVD treatment \n and placebo"),
        col = c("red","blue","green"), 
        lwd = 1,  
        x = 2.3, text.width = 2, bty = "n",
        y = 1.05, cex = 0.8, )
}


