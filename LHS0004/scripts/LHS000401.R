##########################################
##
##      UNC LATIN AMERICAN HEALTH STUDIES
## 
## File: LHS000401.R
## 
## Project: Child mortality in Colombia
##          
## Description: Discrete-time survival analysis
##              to estimate the hazard ratios of
##              determinants of mortality
##
## Programmer: Álvaro Quijano
##
## Date: 10/03/2024
## 	
## Edits:
## 	 - 12/28/24 Update paths to the new folders' structure
## 			Update script to use snakemake workflow
############################################

## Loading libraries
library(haven)
library(tidyverse)

## Load commandLine arguments
args = commandArgs(trailingOnly = TRUE)
print(args)
input_file = args[1]
output_labels = args[3]
output_file = args[2]

## Read data (twice)
data = read_sas(input_file)
data_children = read_sas(input_file)

### Extracting the labels for each variable
labels <- data.frame(
  variable = colnames(data),
  label = map_chr(colnames(data), ~ attr(data %>% pull(.x), "label"))
)
write.csv(labels, output_labels, row.names=FALSE)

## Process the dataset
LHS000401 <- data %>% mutate(STRATA = V022,
                            PSU = V001,
                            HOUSEHOLD_ID = V002,
                            WEIGHT = V005 / 1000000,
                            WOMEN_ID = stringr::str_trim(CASEID),
                            BIRTH_ID = BIDX,
                            REGION = V101,
                            DOB = B3,
                            DOI = V008,
                            YOI = 1900 + (V008-1)/12,
                            AGE_DEATH = B7,
                            AGE_MONTH = DOI - DOB) %>% 
              select(STRATA,PSU,HOUSEHOLD_ID,WEIGHT,WOMEN_ID,DOI,YOI,
                            BIRTH_ID,REGION,DOB,AGE_DEATH,AGE_MONTH)

save(LHS000401, file=output_file)




