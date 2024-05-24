##
## UNC LATIN AMERICAN HEALTH STUDIES
## 
## File: LHS00101.R
## 
## Project: Inequidades en Chile
##
## Programmer: Álvaro Quijano
##
## Date: 05/23/2024
## 
## 

### Call other functions
source('code/LHS000102.R')

## Calling Libraries
library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)

# Reading data
data_2002 = read.csv("../data/censo_edunal_2002.csv", sep = ",") %>% 
          select(EDUCATION_C3,AGE_C7,Male,Female) %>% 
          pivot_longer(cols = c(Male, Female), 
                      names_to = "SEX_C2", 
                      values_to = "N") %>% 
          group_by(EDUCATION_C3,AGE_C7,SEX_C2) %>% 
          summarise(N=sum(N)) %>% 
          select(EDUCATION_C3,AGE_C7,SEX_C2,N) %>% 
          mutate(YEAR=2002)

data_2017 = read.csv("../data/censo_edunal_2017.csv", sep = ",") %>% 
  pivot_longer(cols = c(paste0("X",c(0:18))), 
               names_to = "EDUC", 
               values_to = "N") %>% 
  mutate(N = as.numeric(N)) %>% 
  mutate(EDUCATION_C3 = case_when(
    EDUC %in% c(paste0("X",c(0:8))) ~ 1,
    EDUC %in% c(paste0("X",c(9:12))) ~ 2,
    EDUC %in% c(paste0("X",c(13:18))) ~ 3,
    TRUE ~ NA_real_  # This will handle any values that don't match the given categories
  )) %>%
  select(-EDUC) %>% 
  group_by(REGION,SEX_C2,AGE_C7,X99,EDUCATION_C3) %>%
  summarise(N=sum(N)) %>%  
  ungroup()

### Prorrateo 2017
data_2017 = data_2017 %>% 
  group_by(REGION,SEX_C2,AGE_C7,X99) %>% 
  mutate(Total = sum(N)) %>% 
  ungroup() %>% 
  mutate(percent = N/Total,
         N_ = N + round(X99*percent) ) %>% 
  select(-c(N,Total,percent,X99)) %>% 
  rename(N = N_) %>% 
  select(REGION,EDUCATION_C3,AGE_C7,SEX_C2,N) %>% 
  mutate(YEAR=2017)
    
data_2017_nal = data_2017 %>% 
                group_by(YEAR,EDUCATION_C3,AGE_C7,SEX_C2) %>% 
                summarise(N = sum(N))

### Stacking the dataset
data_r = bind_rows(data_2002,data_2017_nal) %>% 
  group_by(EDUCATION_C3,AGE_C7,SEX_C2) %>%
  group_split() %>% 
  lapply(., calculate_r) %>% 
  bind_rows()

### joining this dataset with 2017
data_2017 = left_join(data_2002, data_r %>% 
                      select(-N) %>% 
                      filter(YEAR==2017))

### projecting the population
data_2017 = data_2017 %>% 
    select(-YEAR) %>% 
    mutate(Y2018 = N*exp(r), 
           Y2019 = N*exp(2*r),
           Y2020 = N*exp(3*r),
           Y2021 = N*exp(4*r),
           Y2022 = N*exp(5*r),
           Y2023 = N*exp(6*r)) %>% 
    rename(Y2017 = N) %>% 
    pivot_longer(cols = starts_with("Y"), names_to = "YEAR", values_to = "N") %>%
   mutate(YEAR = as.numeric(sub("Y", "", YEAR))) %>% 
   select(-r)

### Reading mortality data
deaths = fread("../data/mortalidad.csv")
setnames(deaths, old = "año", new = "YEAR")
deaths = deaths[YEAR %in% c(2017,2019,2021)]

deaths.cancer = deaths %>% 
                mutate(flag = estomago + higado + vesicula + pulmon + colon + mama+ 
                                    prostata+ pancreas + cervix) %>% 
                  filter(flag>0 & YEAR == 2017) %>% select(-flag)

# table(deaths.cancer$nivel_ins ,deaths.cancer$nedu, useNA = "ifany")

### Processing deaths data
deaths_ = deaths %>% as_tibble() %>% 
          mutate(EDUCATION_C3 = case_when(
          nivel_ins %in% c(3,4)  ~ 1,
          nivel_ins == 2 ~ 2,
          nivel_ins == 1 ~ 3,
          nivel_ins == 5  ~ 0,
          TRUE ~ 0), 
          SEX_C2 = ifelse(sexo == 1, "Male","Female"),
          AGE_C7 = cut(edad_cant, 
                       breaks = c(-Inf, 24, 29, 39, 49, 59, 69, Inf), 
                       labels = c(1, 2, 3, 4, 5, 6, 7), 
                       right = TRUE)) %>% 
        rename(REGION = region) %>% 
        select(REGION, EDUCATION_C3, AGE_C7, SEX_C2,
               YEAR, estomago, higado, vesicula, pulmon, colon, mama,       
               prostata, pancreas,cervix) %>% 
        mutate(flag = estomago + higado + vesicula + pulmon + colon + mama+ 
           prostata+ pancreas + cervix) %>% 
        # filter(flag>0) %>% select(-flag) %>%  
        group_by(REGION, EDUCATION_C3, AGE_C7, SEX_C2,
                 YEAR, estomago, higado, vesicula, pulmon, colon, mama,       
                 prostata, pancreas,cervix) %>% 
        summarise(n = n())

## Prorating the database
deaths_ = deaths_ %>% group_by(REGION, AGE_C7, SEX_C2,
                     YEAR, estomago, higado, vesicula, pulmon, colon, mama,       
                     prostata, pancreas,cervix) %>% 
            group_split() %>% 
            lapply(., prorate_values) %>% 
            bind_rows()
