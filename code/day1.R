# SET-UP -------------------------------------------------------------------------------------------

# Clear workspace
rm(list = ls())

# Load core packages
pacman::p_load(dplyr, tidyr, stringr, forcats, magrittr, here, readxl, readr, purrr)

# Load puzzle input
data <- 
  read_csv(
    col_names = "x",
    here::here("data/day1_data.txt")
  )



# PART 1 -------------------------------------------------------------------------------------------

# Fuel required to launch a given module is based on its mass. Specifically, to find the fuel
# required for a module, take its mass, divide by three, round down, and subtract 2.
data %>%
  mutate(fuel = floor(x / 3) - 2) %>%
  summarize(sum(fuel))



# PART 2 -------------------------------------------------------------------------------------------

# Write a function that performs Part 1 step until no more fuel is needed, keeping track of each
# step and then summing at end.
fuel_fn <- 
  function(init_value = NULL) {
    values <- init_value
    iter <- 1
    stop <- F
    while (!stop) {
      new_value <- floor(values[iter] / 3) - 2
      if(new_value <= 0) stop <- T  else values[iter + 1] <- new_value
      iter <- iter + 1
    }
    sum(values[-1])
  }

# Apply function to every value, then sum.
data %>%
  pull(x) %>%
  map_dbl(fuel_fn) %>%
  sum(.)