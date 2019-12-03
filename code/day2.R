# SET-UP -------------------------------------------------------------------------------------------

# Clear workspace
rm(list = ls())

# Load core packages
pacman::p_load(dplyr, tidyr, stringr, forcats, magrittr, here, readxl, readr, purrr)

# Load puzzle input
data <- scan(here::here("data/day2_data.txt"), sep = ",")



# PART 1 -------------------------------------------------------------------------------------------

# Write opcode function
opcode <- function(data_input = NULL, noun_input = NULL, verb_input = NULL) {
  
  # Get dat ready
  data <- data_input
  data[2] <- noun_input
  data[3] <- verb_input
  
  # Run while loop for opcode
  stop <- F
  i <- 1
  while (!stop) {
    if (data[i] == 99 | i > length(data)) stop <- T
    else {
      if (data[i] == 1) {
        out_value <- data[data[i + 1] + 1] + data[data[i + 2] + 1]
      }
      if (data[i] == 2) {
        out_value <- data[data[i + 1] + 1] * data[data[i + 2] + 1]
      }
      data[data[i + 3] + 1] <- out_value
      i <- i + 4
    }
  }
  data[1]
}

# Get result
opcode(data, 12, 2)



# PART 2 -------------------------------------------------------------------------------------------

# Get all possible combinations of values 1-99
combos <- crossing("n" = 0:99, "v" = 0:99)

combo_results <- map2_dbl(
  .x = combos$n,
  .y = combos$v,
  .f = ~ opcode(data, .x, .y)
)

combos[which(combo_results == 19690720), ]