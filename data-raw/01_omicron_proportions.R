library(tidyverse)
library(tidyiddr)

df <- tidyiddr::covariants_country(bust_cache = T)
lookup <- read.csv("data-raw/pango_lookup_covariants.csv")

df %>%
  janitor::clean_names() %>%
  mutate(
    # convert to two week period ending
    week_ending = as.Date(week, origin = "1970-01-01") + 14
  ) %>%
  select(-week) %>%
  pivot_longer(-c(week_ending, country)) %>%
  group_by(week_ending, country) %>%
  # calculate percent of all variants
  mutate(
    value = value * 100 / sum(value, na.rm = T) %>% round(2),
    value = replace(value, value == 0, NA)
  ) %>%
  drop_na() %>%
  # add pango lookup names
  merge(lookup, by.x = "name", by.y = "variant") %>%
  mutate(
    omicron = grepl("omicron", name),
    across(c(country, pango), as.factor)
  ) %>%
  select(-name) %>%
  as_tibble() -> omicron_proportions

variant_list <- sort(unique(omicron_proportions$pango))

usethis::use_data(variant_list, omicron_proportions, overwrite = T)

