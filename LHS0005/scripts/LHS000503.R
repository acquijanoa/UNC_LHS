####################################################################################
##
##                      UNC LATIN AMERICAN HEALTH STUDIES
## 
## File: LHS000503.R
## 
## Project: Process the shapefile
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
library(sf)

## Load commandLine arguments
args <- commandArgs(trailingOnly = TRUE)
input_shapefile <- args[1]
output_shapefile <- args[2]

### Run the functions first
replace_tildes <- function(text) {
  # Define a vector of patterns and replacements
  patterns <- c("á", "é", "í", "ó", "ú", "ü", "ñ", "Á", "É", "Í", "Ó", "Ú", "Ü", "Ñ")
  replacements <- c("a", "e", "i", "o", "u", "u", "n", "A", "E", "I", "O", "U", "U", "N")
  
  # Use gsub with a loop to replace each pattern with its corresponding replacement
  for (i in 1:length(patterns)) {
    text <- gsub(patterns[i], replacements[i], text, fixed = TRUE)
  }
  return(text)
}

## Loading the shapefile
shp_mg = sf::read_sf(input_shapefile) %>% 
  mutate(DHSREGEN = replace_tildes(DHSREGEN), DHSREGSP = replace_tildes(DHSREGSP) ) %>% 
  filter(CNTRYNAMEE == "Honduras") %>% 
  select(-c(CNTRYNAMEE,CNTRYNAMEF, CNTRYNAMES, DHSREGFR,DHSREGSP,REGVAR,REGCODE,REGNAME))

# Convert to uppercase
shp_mg$DHSREGEN = toupper(shp_mg$DHSREGEN)

# Change polygons names
shp_mg = shp_mg %>% mutate(DHSREGEN = ifelse(DHSREGEN == "RESTO FRANCISCO MORAZAN","FRANCISCO MORAZAN",DHSREGEN))
shp_mg = shp_mg %>% mutate(DHSREGEN = ifelse(DHSREGEN == "RESTO CORTES","CORTES",DHSREGEN))

## save in a R object
st_write(shp_mg, output_shapefile, delete_layer = TRUE)
