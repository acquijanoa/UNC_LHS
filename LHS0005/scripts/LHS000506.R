####################################################################################
##
##                      UNC LATIN AMERICAN HEALTH STUDIES
## 
## File: LHS000506.R
## 
## Project: Merge shapefile and produce maps by Education
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
library(ggplot2)

## Load commandLine arguments
args <- commandArgs(trailingOnly = TRUE)
input_file <- args[1]
input_shapefile <- args[2]
input_shapefile2 <- args[3]
input_shapefile3 <- args[4]
output_file <- args[5]

## Read data
data = read.csv(input_file)

# Set the desired order of the factor
data$WOMEN_EDUCATION_C4 <- factor(data$WOMEN_EDUCATION_C4, 
                            levels = c("No education","Primary","Secondary/Highschool","Higher education" ), 
                            ordered = TRUE)

## Read shapefiles
shp = sf::read_sf(input_shapefile)
shp_wd = sf::read_sf(input_shapefile2) %>% filter(FID %in% c(23,69,160,56,95,144)) # Mexico 144
shp_bdr = sf::read_sf(input_shapefile3) %>% filter(ISO == "HN")

## Merging the dataset and the shapefile
shp_st_mgd = shp %>% full_join(data %>% rename(DHSREGEN = REGION), by = "DHSREGEN") 

# Compute centroids for placing text labels
centroids <- shp_st_mgd %>% 
  st_centroid() %>% 
  mutate(
    x = st_coordinates(.)[, 1],
    y = st_coordinates(.)[, 2]
  )

# plot
p1 <- ggplot(shp_st_mgd) + 
  geom_sf(data = shp_wd, inherit.aes = FALSE, alpha = 0.9) +
  geom_sf(aes(fill = PROB), color = "black")  +
  geom_sf(data = shp_bdr, inherit.aes = FALSE, fill = NA, lwd = 0.6, color = "black") +
  geom_text(data = centroids, 
            aes(x = x, y = y, label = round(PROB, 2)), 
            size = 1.2, inherit.aes = FALSE) +
  facet_wrap(~WOMEN_EDUCATION_C4, ncol = 3) +
  ylim(c(12.9, 16.4)) + 
  xlim(c(-89.25, -83)) +
  scale_fill_gradient2(
    low = "yellow",
    mid = "#FFDBBB",
    high = "red", 
    midpoint = .5
  ) + 
  theme_bw() + 
  theme(
    panel.background = element_rect(fill = "aliceblue"), 
    title = element_text(size = 12, face = "bold"),
    legend.position = "bottom", 
    axis.text.x = element_text(size = 6, angle = 0, vjust = 0.5),
    axis.text.y = element_text(size = 6)
  ) + 
  guides(fill = guide_legend(nrow = 1)) + 
  labs(
    title = "Probability of Women Overweight and Obesity in Honduras by level of education, 2019\n", 
    fill = "Probability", x = "", y = ""
  )

# Save map
ggsave(p1, file=output_file, device = "pdf", height = 10.5, width = 14)
