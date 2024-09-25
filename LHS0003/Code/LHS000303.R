# /**********************************************************
#   *                                                        
#   *  PROGRAM NAME: LHS000303.R
#   *                                        
#   *  PROGRAMMER: Álvaro Quijano                                              
#   *
#   *  DATE: 09/25/2024
#   *
#   *---------------------------------------------------------
#   *
#   *  LANGUAGE: R version 4.3.1 
#   *                                                  
#   *  NOTES:   
#   *         - 09/28/23: Processed the data from Guatemala and Nicaragua 
#   *         - 11/19/23: Included the mixed maps
#   *            
#   *---------------------------------------------------------   
#   * 
#   *  INPUT: LHS000302.Rdata
#   *        
#   *  OUTPUT: .pdf and .pes maps 
#   *                                  
#   **********************************************************/
## !!NOTE: RUN the functions below. 

### Loading the libraries
library(ggplot2)
library(magrittr)
library(tidyverse)
library(reshape2)
library(sf)
library(arulesViz)
library(RColorBrewer)
library(ggrepel)


## categorizing the variable
db_pred

## Loading the shapefile
shp_mg = sf::read_sf(paste0("..","/Data/shps/Shp_mgd.shp")) %>% 
                  mutate(DHSREGEN = replace_tildes(DHSREGEN), DHSREGSP = replace_tildes(DHSREGSP) ) %>% 
                  filter(CNTRYNAMEE == "Honduras")
shp_wd = sf::read_sf(paste0("..","/Data/shps/World_Countries_Generalized.shp")) %>% filter(FID %in% c(23,69,160,56,144))
shp_bdr = sf::read_sf(paste0("..","/Data/shps/borders_.shp"))

plot(shp_mg %>% select(DHSREGEN))

shp_mg$DHSREGEN = toupper(shp_mg$DHSREGEN)
shp_mg = shp_mg %>% mutate(DHSREGEN = ifelse(DHSREGEN == "RESTO FRANCISCO MORAZAN","FRANCISCO MORAZAN",DHSREGEN))
shp_mg = shp_mg %>% mutate(DHSREGEN = ifelse(DHSREGEN == "RESTO CORTES","CORTES",DHSREGEN))

### Full join with DHS data
shp_st_mgd = shp_mg %>% full_join(db_pred %>% 
                                          rename(DHSREGEN = REGION), by = "DHSREGEN") 

shp_st_mgd$prob_overweight

p1 <- ggplot(shp_st_mgd) + 
  geom_sf(data = shp_wd, inherit.aes = F, alpha = 0.9) +
  geom_sf(aes(fill = prob_overweight), color = "black")  +
  geom_sf(data = shp_bdr, inherit.aes = F, fill = NA, lwd = 0.6, color = "black") +
  facet_wrap(~windex5, ncol=3) +
  ylim(c(12.8,16.4)) + 
  xlim(c(-90.05,-83)) +
  scale_fill_gradient2(
    low = "yellow",
    mid = "#FFDBBB",
    high = "red", 
    midpoint = .5
  ) + 
  theme_bw() + 
  theme(panel.background = element_rect(fill = "aliceblue"), 
        title = element_text(size = 12, face = "bold"),
        legend.position = "bottom", 
        axis.text.x = element_text(size = 6, angle = 0, vjust = 0.5),
        axis.text.y = element_text(size = 6)) + 
  guides(fill = guide_legend(nrow = 1)) + 
  labs(title = "Probability of Women Overweight and Obesity in Honduras by Wealth Status, 2019\n", 
       fill = "Probability", x = "", y = "")

p1
### Maps
ggsave(p1, file="output/Prevalence_stunting_map.eps", device = "eps")
ggsave(p1, file="output/Prevalence_stunting_map.pdf", device = "pdf", width = 14)

### overnut
data_ov = data %>% 
  select(-departmentId) %>% 
  melt(.,id.vars = c("country","department")) %>% 
  separate(col = variable, 
           into = c("variable","year")) %>% 
  filter(variable == "overnut")  %>% 
  mutate(year = 2000 + as.numeric(year)) %>% 
  filter(year != 2012)

data_ov_gn = data_gn0 %>% select(-departmentId) %>% 
  melt(., id.vars = c("country","department","year")) %>%
  filter(variable == "overnut") %>% 
  select(country,department,variable, year, value)

db_ov = union(data_ov,data_ov_gn)

db_ov = db_ov %>% mutate(value_c = cut(value, 
                                 breaks = c(0,15,25,35,45,50,55,60,100), 
                                 labels = c("<15","15-24","25-34",
                                            "35-44","45-49","50-54",
                                            "55-59","60+"),
                                 include.lowest = T, right = F), 
                   year_c = cut(year, 
                                breaks = c(0,2000,2010,10000), 
                                labels = c("<2000","2000-2009","2010-2019"),
                                include.lowest = T, right = F)) %>%  as_tibble(.)


### Full join with DHS data
shp_ov_mgd = shp_mgd %>% full_join(db_ov %>% 
                                     rename(DHSREGEN = department), by = "DHSREGEN") 


### Maps
pal <- rev(brewer.pal(9, "RdYlGn"))[-c(1,2,11)]
p2 = ggplot(shp_ov_mgd) + 
  geom_sf(data = shp_wd, inherit.aes = F, alpha = 0.9) +
  geom_sf(aes(fill = value_c), color = "black")  +
  geom_sf(data = shp_bdr, inherit.aes = F, fill = NA, lwd = 0.6, color = "black") +
  geom_text_repel(data = dt_label, 
                  mapping = aes(x = X, y = Y, label = NAME), 
                  inherit.aes = FALSE, show.legend = F, size = 3,
                  bg.color = "white", bg.r = 0.25) +
  facet_grid(~year_c) +
  scale_fill_manual(values = pal) + 
  ylim(c(11,17.6)) + 
  xlim(c(-93.05,-83)) +
  theme_bw() + 
  theme(panel.background = element_rect(fill = "aliceblue"), 
        title = element_text(size = 12, face = "bold"),
        legend.position = "bottom", 
        axis.text.x = element_text(size = 6, angle = 0, vjust = 0.5),
        axis.text.y = element_text(size = 6)) + 
  guides(fill = guide_legend(nrow = 1)) + 
  labs(title = "   Prevalence of overweight/obesity among women of reproductive age, Honduras, Nicaragua and Guatemala\n", 
       fill = "% women with \n overweight/obesity", x = "", y = "") 
p2
ggsave(p2, file="output/Prevalence_overweight_map.eps", device = "eps")
ggsave(p2, file="output/Prevalence_overweight_map.pdf", device = "pdf", width = 12)


### nt
data_nt = data %>% 
  select(-country, -departmentId) %>% 
  melt(.,id.vars = "department") %>% 
  separate(col = variable, into = c("variable","year")) %>% 
  spread(key = variable, value = value) %>% 
  mutate(nt = ifelse(stunting > 15 & overnut < 35, "Stage 1 (beginning)",
                     ifelse(stunting > 15 & overnut > 35,"Stage 2 (double-burden)",
                            ifelse(stunting < 15 & overnut > 35,"Stage 3 (completed)","other")))) %>% 
  select(department, year, nt)

### Full join with DHS data

shp_hnd_nt_mgd = shp_hnd %>% full_join(data_nt %>% rename(DHSREGEN = department))

###
db_0 = db %>% select(country,department,variable,value,year_c) %>% 
          rename(stunting = value, stunting_var = variable)
db_ov_0 = db_ov %>% select(country,department,variable,value,year_c) %>% 
          rename(overnut = value, overnut_var = variable)

db_mgd = data.frame(cbind(db_0, db_ov_0)) %>% 
              mutate(nt = ifelse(stunting > 20 & overnut <= 40, "Stage 1 (beginning)",
                      ifelse(stunting > 20 & overnut > 40,"Stage 2 (double-burden)",
                      ifelse(stunting <= 20 & overnut > 40,"Stage 3 (completed)","other")))) %>% 
          select(department, year_c, nt)

#### 
shp_hnd_nt_mgd = shp_mgd %>% full_join(db_mgd %>% 
                                     rename(DHSREGEN = department), by = "DHSREGEN") 

### Maps
pal <- c("#fcf8a9","#dc4b31","#f3b660")

p3 = ggplot(shp_hnd_nt_mgd) + 
  geom_sf(data = shp_wd, inherit.aes = F, alpha = 0.9) +
  ylim(c(11,17.6)) + 
  xlim(c(-93.05,-83)) +
  geom_sf(aes(fill = nt), color = "black") + theme_void() + 
  geom_sf(data = shp_bdr, inherit.aes = F, fill = NA, lwd = 0.6, color = "black") +
  geom_text_repel(data = dt_label, 
                  mapping = aes(x = X, y = Y, label = NAME), 
                  inherit.aes = FALSE, show.legend = F, size = 3,
                  bg.color = "white", bg.r = 0.25) +
  labs(title = "   The nutrition transition by stage and department, Honduras, Nicaragua and Guatemala \n", 
       fill = "Stage of nutrition transition", x = "", y = "") + 
  facet_wrap(~year_c) +  
  theme_bw() + 
  scale_fill_manual(values = pal) +
  theme(panel.background = element_rect(fill = "aliceblue"), 
        title = element_text(size = 12, face = "bold"),
        legend.position = "bottom")  + 
  guides(fill = guide_legend(nrow = 1)) 

p3

ggsave(p3, file="output/Nutrition_transition_map.eps", device = "eps")
ggsave(p3, file="output/Nutrition_transition_map.pdf", device = "pdf")


pal2 <- c("#fcf8a9","#f3b660","#dc4b31")
p4 = ggplot(shp_hnd_nt_mgd) + 
  geom_sf(data = shp_wd, inherit.aes = F, alpha = 0.9) +
  ylim(c(11,17.6)) + 
  xlim(c(-93.05,-83)) +
  geom_sf(aes(fill = nt), color = "black") + theme_void() + 
  geom_sf(data = shp_bdr, inherit.aes = F, fill = NA,
              lwd = 0.6, color = "black") +
  geom_text_repel(data = dt_label, 
                  mapping = aes(x = X, y = Y, label = NAME), 
                  inherit.aes = FALSE, show.legend = F, size = 3,
                  bg.color = "white", bg.r = 0.25) +
  labs(title = "   The nutrition transition by stage and department, Honduras, Nicaragua and Guatemala \n", 
       fill = "Stage of nutrition transition", x = "", y = "") + 
  facet_wrap(~year_c) +  
  theme_bw() + 
  scale_fill_manual(values = pal2) +
  theme(panel.background = element_rect(fill = "aliceblue"), 
        title = element_text(size = 12, face = "bold"),
        legend.position = "bottom")  + 
  guides(fill = guide_legend(nrow = 1)) 

p4

ggsave(p4, file="output/Nutrition_transition_map_2.eps", device = "eps")
ggsave(p4, file="output/Nutrition_transition_map_2.pdf", device = "pdf")


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




