# SET-UP -------------------------------------------------------------------------------------------

# Clear workspace
rm(list = ls())

# Load core packages
pacman::p_load(dplyr, tidyr, stringr, magrittr, here, readr, purrr)

# Load puzzle input and convert to vector.
data <- read_lines(here::here("data/day8_data.txt")) %>%
  str_split(., pattern = "", simplify = T) %>%
  as.numeric()



# BOTH PARTS ---------------------------------------------------------------------------------------

# Images is 25 X 6 so split data every 150 values.
data_list <- data %>% 
  split(rep(1:(length(data) / 150), each = 150)) 



# PART 1 -------------------------------------------------------------------------------------------

# Count number of 0s in each layer; find the layer with the fewest, then get product of 1s and 2s 
zero_counts_vec <- map_dbl(data_list, ~ sum(.x == 0))
min_zero <- which.min(zero_counts_vec)
sum(data_list[[min_zero]] == 1) * sum(data_list[[min_zero]] == 2)



# PART 2 -------------------------------------------------------------------------------------------

# Turn each layer into an indexed (by layer and position)
layers <- data_list %>% 
  map(as.data.frame) %>%
  map(~ gather(.x, key = position)) %>%
  map(~ mutate(.x, position = row_number())) %>%
  bind_rows(.id = "index")

# Turn all 2s into NAs and then sort and take first non-NA value
layers %<>%
  mutate(
    value = na_if(value, 2), 
    index = as.numeric(index)
  ) %>%
  group_by(position) %>%
  arrange(index) %>%
  summarize(outcome = first(na.omit(value))) %>%
  ungroup() 

# Add x and y coordinates to data and plot
layers %<>%
  mutate(
    x_coord = rep(1:25, times = 6),
    y_coord = rep(6:1, each = 25),
    outcome = factor(outcome)
  )

library(ggplot2)
windows(6, 1.5)
ggplot(data = layers, aes(x = x_coord, y = y_coord, label = outcome, color = outcome)) +
  geom_point(size = 4, shape = 15) + 
  scale_color_manual(values = c("black", "white")) +
  theme_minimal()
