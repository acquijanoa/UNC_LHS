##########################################
##
##      UNC LATIN AMERICAN HEALTH STUDIES
## 
## File: LHS000301.R
## 
## Project: Women obesity in Honduras 
##
## Description: 
##
## Programmer: Álvaro Quijano
##
## Date: 09/24/2024
## 
############################################

## Uploading libraries
library(haven)
library(dplyr)
library(magrittr)

### Loading data
women_hon <- read_sav("../Data/wm.sav") ### Women from 15 to 49 years old

### Extracting the labels
variable_labels_wm <- lapply(women_hon, function(x) attr(x, "label"))
db_labels_wm <- data.frame(variable = names(variable_labels_wm), 
                           label = unlist(paste(variable_labels_wm,1:597) ), 
                           stringsAsFactors = FALSE)
# db_labels_wm %>% View()

## Embarazada
women_hon$CP1
table(women_hon$CP1,  
      useNA = "always")

women_hon$DV1A
table(women_hon %>%  pull(VT22A),  
          useNA = "always")

women_hon$MSTATUS
table(women_hon %>%  pull(MA6),  useNA = "always")

# En los hogares entrevistados, se identificaron 20 841
# mujeres (de 15-49 años de edad). De estas, 19 279 fueron
# entrevistadas, llevando a una tasa de respuesta de 93 por
# ciento dentro de los hogares entrevistados.
# 1562 (NA) mujeres no respondieron

### Deriving variables in the output dataset
LHS000301 <- women_hon %>% mutate(
  BMI = case_when(WW8 == 999.8 | WW7 == 999.8 ~ NA,   
                  is.na(WW7) | is.na(WW8) ~ NA,
                  TRUE ~ WW7/(WW8/100)^2),
  OVERWEIGHT = case_when(is.na(BMI) ~ NA,
                         BMI >=25  ~ 1,
                         BMI < 25 ~ 0), 
  WOMEN_AGE_C4 = cut(WB4, breaks = c(15,seq(20,50,by=10)), 
                     right = F, 
                     include.lowest = T),
  WOMEN_AGE_C7 = cut(WB4, breaks = seq(15,50,by=5), 
                     right = F, 
                     include.lowest = T),
  WOMEN_AGE_C6 = cut(WB4, breaks = c(seq(15,40,by=5),50), 
                     right = F, 
                     include.lowest = T),
  REGION = HH7,
  WOMEN_EDUCATION_C4 = case_when(
    welevel == 0 ~ 0,
    welevel %in% c(1,2) ~ 1,
    welevel %in% c(3,4) ~ 2,
    welevel %in% c(5) ~ 3
  ),
  WOMEN_EDUCATION_C3 = case_when(
    welevel %in% c(0,1,2) ~ 0,
    welevel %in% c(3,4) ~ 1,
    welevel %in% c(5) ~ 2
  ),
  URBAN = case_when(HH6 == 1 ~ 1,
                    HH6 == 2 ~ 0,
                    is.na(HH6) ~ NA),
  DOMAIN = ifelse(is.na(BMI) + is.na(WOMEN_AGE_C7) == 0 & CP1 ==2 ,1,0)) %>% 
  select(PSU,stratum, wmweight, BMI,OVERWEIGHT,WOMEN_AGE_C4, WOMEN_AGE_C7,WOMEN_AGE_C6,DOMAIN,windex5, REGION,
         URBAN, WOMEN_EDUCATION_C4, WOMEN_EDUCATION_C3,MSTATUS,ethnicity)

## Creating the labels
LHS000301$REGION <- factor(LHS000301$REGION, 
                           levels = 1:20, 
                           labels = c("ATLANTIDA", "COLON", "COMAYAGUA", "COPAN", 
                                      "CORTES", "CHOLUTECA", "EL PARAISO", 
                                      "FRANCISCO MORAZAN", "GRACIAS A DIOS", "INTIBUCA", 
                                      "ISLAS DE LA BAHIA", "LA PAZ", "LEMPIRA", 
                                      "OCOTEPEQUE", "OLANCHO", "SANTA BARBARA", 
                                      "VALLE", "YORO", "SAN PEDRO SULA", "DISTRITO CENTRAL"))
LHS000301$URBAN <- factor(LHS000301$URBAN, levels = 0:1,labels=c("Rural","Urban"))
LHS000301$WOMEN_EDUCATION_C4 <- factor(LHS000301$WOMEN_EDUCATION_C4, levels = 0:3,labels=c("No education",
                                                                                           "Primary",
                                                                                           "Secondary/Highschool",
                                                                                           "Higher education"))
LHS000301$WOMEN_EDUCATION_C3 <- factor(LHS000301$WOMEN_EDUCATION_C3, levels = 0:2,labels=c("No education/Primary",
                                                                                         "Secondary/Highschool","Higher education"))
LHS000301$windex5 <- factor(LHS000301$windex5, levels = 1:5,labels=c("Poorest","Poorer","Middle","Richer","Richest"))

save(LHS000301, file="../Data/LHS000301.Rdata")

