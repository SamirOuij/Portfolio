library(tidyverse)
install.packages(c("choroplethr", "choroplethrMaps")) 
library(choroplethr)
library(choroplethrMaps)
install.packages("ggpubr")
library(ggpubr)


prison_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

#Trends over time chart
Counties_Black_Pop <- select(prison_data, state, year, county_name ,total_pop_15to64,black_pop_15to64)
Cook_County_Black_Pop <- filter(Counties_Black_Pop, county_name == "Cook County", state == "IL")
Cook_County_Black_Pop <- mutate(Cook_County_Black_Pop, percent_black_15to64 = 100 *(black_pop_15to64 / total_pop_15to64))
graph_data <- Cook_County_Black_Pop %>% filter(!is.na(Cook_County_Black_Pop$percent_black_15to64))

 graph_data %>%
  ggplot(aes(x = year, y = percent_black_15to64)) +
  geom_line() +
  ggtitle("Percent Black in Cook County Jails")
 
 
 #Map
  us_states <- map_data("state")
  us_counties <-map_data("county")
  prison_data_lowercase <- prison_data %>% mutate(county_name = tolower(county_name))
  prison_data_lowercase <- prison_data_lowercase %>% mutate(county_name = (gsub("county", "", county_name)))
  prison_data_lowercase <- rename(prison_data_lowercase, subregion = county_name)
  prison_data_lowercase_2018 <- filter(prison_data_lowercase, year == "2018")
  us_counties_2018 <- right_join(prison_data_lowercase_2018,us_counties, by = "subregion")
  prison_data_lowercase_2018 = subset(prison_data_lowercase_2018, select = -c(region) )
  prison_data_lowercase_2018 <- rename(prison_data_lowercase_2018, region = fips)
  prison_data_lowercase_2018 <- rename(prison_data_lowercase_2018, value  = male_jail_pop)
  prison_data_lowercase_2018 <- select(prison_data_lowercase_2018, region, value )
  county_choropleth(prison_data_lowercase_2018, title = "2018 Male Jail Populations by County",
  legend = "Male Jail Population")
                    
  
  #Two Variable Comparison
  scatter_plot_prep_WA <- prison_data %>% filter(!is.na(prison_data$land_area), year ==2000, state == "WA" )
  scatter_plot_prep_WA <- scatter_plot_prep_WA %>% mutate(percent_jailed = (total_jail_pop / total_pop) * 100)
  scatter_plot_prep_WA$percent_jailed
  land_area_vs_percent_jailed_Washington <- ggplot(scatter_plot_prep_WA, aes(x = land_area, y = percent_jailed)) + geom_point() + xlim(0, 4500)
  
  scatter_plot_prep_CA <- prison_data %>% filter(!is.na(prison_data$land_area), year ==2000, state == "CA" )
  scatter_plot_prep_CA <- scatter_plot_prep_CA %>% mutate(percent_jailed = (total_jail_pop / total_pop) * 100)
  land_area_vs_percent_jailed_CA<- ggplot(scatter_plot_prep_CA, aes(x = land_area, y = percent_jailed)) + geom_point() + xlim(0, 11000)
  
  scatter_plot_prep_OR <- prison_data %>% filter(!is.na(prison_data$land_area), year ==2000, state == "OR" )
  scatter_plot_prep_OR <- scatter_plot_prep_OR %>% mutate(percent_jailed = (total_jail_pop / total_pop) * 100)
  land_area_vs_percent_jailed_OR<- ggplot(scatter_plot_prep_OR, aes(x = land_area, y = percent_jailed)) + geom_point() + xlim(0, 11000)

  
  figure <- ggarrange(land_area_vs_percent_jailed_CA, land_area_vs_percent_jailed_Washington, land_area_vs_percent_jailed_OR,
                      labels = c("California", "Washington", "Oregon"),
                      ncol = 2, nrow = 2)
  annotate_figure(figure, top = text_grob("Land Area vs Percent Jailed", 
                                        color = "red", face = "bold", size = 14))
  
  
  
  # 5 Variables
  
  prison_data_extended <-mutate(prison_data, percent_jailed = (total_jail_pop / total_pop) * 100)
  WA_Counties_Pop <- select(prison_data_extended, year, state, county_name, total_pop)
  King_County_Pop <- filter(WA_Counties_Pop, county_name == "King County", state == "WA" )  
  
  max_king_county_pop <- max(King_County_Pop$total_pop)
  max_king_county_pop #2233163
  
  min_king_county_pop <-min(King_County_Pop$total_pop)
  min_king_county_pop #1138907
  
  king_county_pop_difference <- max_king_county_pop - min_king_county_pop
  king_county_pop_difference #1094256
  
  Snoho_County_Pop <- filter(WA_Counties_Pop, county_name == "Snohomish County", state == "WA" )  
  max_snoho_county_pop <- max(Snoho_County_Pop$total_pop)
  max_snoho_county_pop #814901
  
  min_snoho_county_pop <-min(Snoho_County_Pop$total_pop)
  min_snoho_county_pop #262647
  
  king_snoho_pop_difference <- max_snoho_county_pop - min_snoho_county_pop
  king_snoho_pop_difference #55254
  
 
  
  