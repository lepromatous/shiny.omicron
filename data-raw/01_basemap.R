library(tidyverse)
library(sf)

rworldmap::getMap(resolution = "low") %>% 
    st_as_sf() %>%
    select(NAME) %>%
    mutate(NAME = case_when(
      NAME == "Bosnia and Herz." ~ "Bosnia and Herzegovina",
      NAME == "Cape Verde" ~ "Cabo Verde", 
      NAME == "Congo (Kinshasa)" ~ "Republic of the Congo",
      NAME == "Congo (Brazzaville)" ~ "Democratic Republic of the Congo",
      NAME == "Czech Rep." ~ "Czech Republic",
      NAME == "Dominican Rep." ~ "Dominican Republic",
      NAME == "Ivory Coast" ~ "Côte d'Ivoire",
      NAME == "Macedonia" ~ "North Macedonia",
      NAME == "N. Korea" ~ "North Korea",
      NAME == "S. Sudan" ~ "South Sudan",
      NAME == "Solomon Is." ~ "Solomon Islands",
      NAME == "S. Korea" ~ "South Korea",
      NAME == "Swaziland" ~ "Eswatini",
      NAME == "United States" ~ "USA",
      TRUE ~ as.character(NAME)
    )) %>%
    mutate(NAME = as.character(NAME)) %>%
    rename(country = NAME) %>%
    st_transform(crs = "ESRI:54019") -> basemap

usethis::use_data(basemap, overwrite = T)

