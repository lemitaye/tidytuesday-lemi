
# Analysis of names from 1880 to 2010

library(tidyverse)
theme_set(theme_light())

# Read one file for test
yob1880 <- read_csv(
  file = "./data/babynames/yob1880.txt",
  col_names = FALSE,
  col_types = cols(
    X1 <- col_character(),
    X2 <- col_character(),
    X3 <- col_double()
  )
)
yob1880

# Let's create a function that reads all data, combine it and return all
# in one tibble

read_babynames <- function() {
  # create an empty tibble
  babynames <- tibble()

  # iterate to read all files into a single data frame
  for (year in 1880:2010) {
    # construct path string
    path <- sprintf("./data/babynames/yob%d.txt", year)
    # read file
    current <- read_csv(
      file = path,
      col_names = FALSE,
      col_types = cols(
        X1 <- col_character(),
        X2 <- col_character(),
        X3 <- col_double()
      )
    )
    # create a year column and supply year
    current["year"] <- year

    # append to 'babynames' data
    babynames <- rbind(babynames, current)
  }

  names(babynames) <- c("names", "sex", "count", "year")
  assign("babynames", babynames, envir = globalenv())
}

read_babynames()
babynames

# Now, to analysis...
babynames %>%
  group_by(year, sex) %>%
  count(wt = count) %>%
  ggplot(aes(year, n, color = sex)) +
  geom_line()

babynames %>%
  group_by(year, sex) %>%
  mutate(
    prop = count / sum(count, na.rm = TRUE),
    # sanity check
    N = sum(prop)
  ) # %>%
# ungroup() %>%
# count(N)

top1000 <- babynames %>%
  group_by(year, sex) %>%
  mutate(
    rank = row_number(desc(count)),
    prop = count / sum(count, na.rm = TRUE)
  ) %>%
  filter(rank <= 1000) %>%
  select(-rank) %>% 
  ungroup()

boy <- top1000 %>%
  filter(sex == "M")

girl <- top1000 %>%
  filter(sex == "F")

subset <- top1000 %>%
  group_by(year, names) %>%
  count(wt = count) %>%
  filter(names %in% c("John", "Harry", "Mary", "Marilyn"))

ggplot(subset, aes(year, n)) +
  geom_line(size = 1) +
  facet_wrap(~names, nrow = 4, scales = "free")

# Measuring the increase in naming diversity
top1000 %>%
  group_by(year, sex) %>%
  summarise(prop_sum = sum(prop, na.rm = TRUE)) %>%
  ggplot(aes(year, prop_sum)) +
  geom_line(aes(color = sex)) +
  coord_cartesian(ylim = c(0, 1.2))

# For 2010
prop_cumsum2010 <- (boy %>%
  filter(year == 2010) %>%
  arrange(desc(prop)) %>%
  mutate(prop_cumsum = cumsum(prop))
)[["prop_cumsum"]]

detect_index(prop_cumsum2010, ~ . >= 0.5)

# For 1900 ...
prop_cumsum1900 <- (boy %>%
  filter(year == 1900) %>%
  arrange(desc(prop)) %>%
  mutate(prop_cumsum = cumsum(prop))
)[["prop_cumsum"]]

detect_index(prop_cumsum1900, ~ . >= 0.5)

# Nest and apply
year_sex <- top1000 %>%
  group_by(year, sex) %>%
  nest()
year_sex

quantile_index <- function(df, q = 0.5) {
  (df %>%
    arrange(desc(prop)) %>%
    mutate(prop_cumsum = cumsum(prop)))[["prop_cumsum"]] %>%
    detect_index(~ . >= q)
}

# Test ...
data0 <- year_sex$data[[42]]
quantile_index(data0)

year_sex <- year_sex %>%
  mutate(q50 = map_int(data, quantile_index))

year_sex

ggplot(year_sex) +
  geom_line(aes(year, q50, color = sex))

# The "last letter" revolution

subtable <- babynames %>%
  mutate(last_letter = str_sub(names, -1)) %>%
  filter(year %in% c(1910, 1960, 2010)) %>%
  group_by(year, sex, last_letter) %>%
  count(name = "Sum", wt = count) %>% 
  ungroup()
subtable

subtable %>%
  group_by(sex, year) %>%
  summarise(tot_count = sum(Sum))

subtable %>%
  group_by(sex, year) %>%
  mutate(letter_prop = Sum / sum(Sum)) %>%
  ungroup() %>%
  ggplot() +
  geom_col(aes(last_letter, letter_prop, fill = as_factor(year)),
    position = "dodge"
  ) +
  facet_wrap(~sex, nrow = 2)

dny_ts <- babynames %>%
  mutate(last_letter = str_sub(names, -1)) %>%
  group_by(year, sex, last_letter) %>%
  mutate(Sum = sum(count)) %>%
  ungroup() %>%
  group_by(sex, year) %>%
  mutate(letter_prop = Sum / sum(count)) %>%
  ungroup() %>%
  filter(last_letter %in% c("d", "n", "y"), sex == "M") %>%
  select(year, last_letter, letter_prop) %>%
  distinct()

ggplot(dny_ts, aes(year, letter_prop, color = last_letter)) +
    geom_line()

# Boy names that became girl names (and vice versa)

lesley_like <- (top1000 %>%
  distinct(names) %>%
  mutate(lower = str_to_lower(names)) %>%
  filter(str_detect(lower, "lesl")))[["names"]]
lesley_like

top1000 %>% 
    filter(names %in% lesley_like) %>% 
    count(names, wt = count)

top1000 %>% 
    filter(names %in% lesley_like) %>% 
    count(year, sex, wt = count) %>% 
    group_by(year) %>% 
    mutate(norm = n/sum(n)) %>% 
    ungroup() %>% 
    ggplot(aes(year, norm)) +
    geom_line(aes(linetype = sex))












