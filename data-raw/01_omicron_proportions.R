library(tidyverse)
library(tidyiddr)

df <- tidyiddr::covariants_country(bust_cache = T)
lookup <- read.csv("data-raw/pango_lookup_covariants.csv")

add_pango <- function(.data) {
  .data %>%
    drop_na() %>%
    # add pango lookup names
    merge(lookup, by.x = "name", by.y = "variant", all.x = F) %>%
    mutate(
      omicron = grepl("omicron", name),
      pango =  as.factor(pango)
    ) %>%
    select(-name) %>%
    as_tibble()
}

df %>%
  janitor::clean_names() %>%
  mutate(
    # convert to two week period ending
    week_ending = as.Date(week, origin = "1970-01-01") + 14
  ) %>%
  select(-week) %>%
  pivot_longer(-c(week_ending, country)) -> tmp1

tmp1 %>%
  group_by(week_ending, country) %>%
  # calculate percent of all variants
  mutate(
    value = value * 100 / sum(value, na.rm = T) %>% round(2),
    value = replace(value, value == 0, NA),
    country = as.factor(country)
  ) %>%
  add_pango() -> omicron_proportions

tmp1 %>% 
  group_by(week_ending) %>%
  mutate(total = sum(value, na.rm = T)) %>%
  ungroup() %>%
  group_by(week_ending, name) %>%
  summarise(
    value = sum(value, na.rm = T) / first(total) %>% round(2) %>% replace(. == 0, NA)
  ) %>%
  ungroup() %>%
  add_pango() %>%
  subset(pango != "") -> omicron_global_proportions
  
variant_list  <- sort(unique(omicron_proportions$pango))

dates_list <- sort(unique(omicron_proportions$week_ending))

usethis::use_data(variant_list, omicron_proportions, dates_list, overwrite = T)
usethis::use_data(omicron_global_proportions, overwrite = T)

