####################################################################################
##
##                      UNC LATIN AMERICAN HEALTH STUDIES
## 
## File: LHS000507.R
## 
## Project: Calculate the appropiate transofmrations to the response by wealth, age
##              and educational level
##
## Description: 
##
## Programmer: Álvaro Quijano
##
## Date: 01/11/2025
## 
######################################################################################

  # Upload library
library(dplyr)
library(ggplot2)

## Load commandLine arguments
args <- commandArgs(trailingOnly = TRUE)
  input_file1 <- args[1]
  input_file2 <- args[2]
  input_file3 <- args[3]
  output_file1 <- args[4]
  output_file2 <- args[5]
  output_file3 <- args[6]

  # Define the logit function and its variance (Delta method)
logit <- function(x){ log(x/(1-x)) }
var_logit <- function(x, var_x){ var_x / ((x^2)*(1-x)^2) }

 # Define the arcsin square root transformation and its asymptotic variance (Raghunattan, 2007)
arcsin_sq <-function(x){ asin(sqrt(x))}   
var_arcsin_sq <- function(x, var_x){ ((x*(1-x))/(var_x))} 

  ##  Load data and create the new variable (wealth)
data1 <- read.csv(input_file1)
data_transformed1 <- data1 %>% 
    mutate(yM = logit(PROB), 
            yM.var = var_logit(PROB, VAR),
           yR = arcsin_sq(PROB),
            yR.var = var_arcsin_sq(PROB,VAR)) %>%   
    select(REGION,WINDEX,yM,yM.var,yR,yR.var)

# write the file
write.csv(data_transformed1, file = output_file1)

##  Load data and create the new variable (age)
data2 <- read.csv(input_file2)
data_transformed2 <- data2 %>% 
  mutate(yM = logit(PROB), 
         yM.var = var_logit(PROB, VAR),
         yR = arcsin_sq(PROB),
         yR.var = var_arcsin_sq(PROB,VAR)) %>%   
  select(REGION,WOMEN_AGE_C4,yM,yM.var,yR,yR.var)

# write the file
write.csv(data_transformed2, file = output_file2)

##  Load data and create the new variable (education)
data3 <- read.csv(input_file3)
data_transformed3 <- data3 %>% 
  mutate(yM = logit(PROB), 
         yM.var = var_logit(PROB, VAR),
         yR = arcsin_sq(PROB),
         yR.var = var_arcsin_sq(PROB,VAR)) %>%   
  select(REGION,WOMEN_EDUCATION_C4,yM,yM.var,yR,yR.var)

# write the file
write.csv(data_transformed3, file = output_file3)

