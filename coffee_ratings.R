
# Analysis of coffee ratings data
# Date: 2020-08-30


library(tidytuesdayR)
library(tidyverse)
library(ggridges)
theme_set(theme_light()) # to have a light theme in ggplot2 graphs

# laod dataset
tuesdata <- tt_load("2020-07-07")
coffee_ratings <- tuesdata$coffee_ratings %>% 
  filter(total_cup_points > 0)

# Write it to csv for an offline use (contingency)
# write_csv(coffee_ratings, "./data/coffee_ratings.csv")

coffee_ratings <- read_csv("./data/coffee_ratings.csv") # for offline use

# Compare count by country
coffee_ratings %>%
  filter(!is.na(country_of_origin)) %>%
  mutate(country_of_origin = factor(country_of_origin) %>% fct_lump(16)) %>%
  count(country_of_origin, sort = TRUE)


coffee_ratings %>%
  filter(!is.na(country_of_origin)) %>%
  count(country_of_origin = fct_lump(country_of_origin, 16), sort = TRUE) %>%
  ggplot(aes(fct_reorder(country_of_origin, n), n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Country of Origin", y = "Count")

# Arabicas are the most dominant species
coffee_ratings %>%
  count(species)

# How is variety related to total cup points?
coffee_ratings %>%
  filter(!is.na(variety), total_cup_points > 10) %>%
  mutate(variety = fct_lump(variety, 10), sort = TRUE) %>%
  ggplot(aes(total_cup_points, fct_reorder(variety, total_cup_points))) +
  geom_boxplot()

# Select vars and set a uniqe identifier
coffee_selected <- coffee_ratings %>%
  mutate(coffee_id = row_number()) %>%
  select(coffee_id, total_cup_points:farm_name, variety, aroma:moisture)

# Compare countries by points
coffee_selected %>%
  group_by(country_of_origin) %>%
  summarise(
    mean_rating = mean(total_cup_points, na.rm = TRUE),
    median_rating = median(total_cup_points, na.rm = TRUE),
    n = n()
  ) %>%
  select(country_of_origin, mean_rating, median_rating, n) %>%
  arrange(desc(median_rating, mean_rating))

# Ethiopian coffee is at the top w.r.t median rating (yee!)
coffee_selected %>%
  filter(!is.na(country_of_origin), total_cup_points > 65) %>%
  mutate(country_of_origin = fct_lump(country_of_origin, 16), sort = TRUE) %>%
  ggplot(aes(
    total_cup_points,
    fct_reorder(country_of_origin, total_cup_points)
  )) +
  geom_boxplot()

# Analysis of ratings
coffee_ratings %>%
  count(producer, sort = TRUE)
# about 82% of producers are non NAs
coffee_ratings %>% 
  summarise( across( everything(), ~ mean(!is.na(.)) ) ) %>% 
  gather()

coffee_ratings %>% 
  count(company, sort = TRUE)

coffee_ratings %>% 
  count(color, sort = TRUE)

# Analysis by country
coffee_ratings %>% 
  count(country = fct_lump(country_of_origin, 16), sort = TRUE) %>% 
  filter(!is.na(country)) %>% 
  mutate(country = fct_reorder(country, n)) %>% 
  ggplot(aes(n, country)) +
  geom_col()


# Analysis of metrics
coffee_ratings %>%
  mutate(coffee_id = row_number()) %>%
  select(coffee_id, total_cup_points, variety, company,
         country_of_origin, aroma:cupper_points) %>% 
  pivot_longer(aroma:cupper_points, names_to = "metric", values_to = "value") %>% 
  group_by(coffee_id, total_cup_points) %>% 
  summarise(total = sum(value)) %>% 
  ggplot(aes(total_cup_points, total)) +
  geom_point()

# Brand new selection
coffee_metrics <- coffee_ratings %>%
  mutate(coffee_id = row_number()) %>%
  select(coffee_id, total_cup_points, variety, company,
         country_of_origin, aroma:cupper_points) %>% 
  pivot_longer(aroma:cupper_points, names_to = "metric", values_to = "value")

coffee_metrics %>% 
  mutate(metric = fct_reorder(metric, value)) %>% 
  ggplot(aes(value, metric)) +
  geom_density_ridges()

coffee_metrics %>% 
  group_by(metric) %>% 
  summarise(average = mean(value),
            sd = sd(value)) %>% 
  arrange(desc(average))
  
# Checkout the 'widyr' package (26:06 on the video)
# 'ggraph' & 'igraph' (28:30)

install.packages(c("widyr", "ggraph", "igraph"))













