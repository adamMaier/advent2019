# SET-UP -------------------------------------------------------------------------------------------

# Clear workspace
rm(list = ls())

# Load core packages
pacman::p_load(dplyr, tidyr, stringr, forcats, magrittr, here, readxl, readr, purrr)

# Load puzzle input
data <- 
  read_csv(here::here("data/day3_data.txt"), col_names = F) %>%
  as.matrix()



# PART 1 -------------------------------------------------------------------------------------------

# Write a function that, for each move of the wire, binds entries to a vector on all positions
# passed.
path_gen <- function(input_d = NULL) {
  position <- c("0,0")
  for (i in 1:length(input_d)) {
    direction <- substr(input_d[i], 1, 1)
    paces <- as.numeric(substr(input_d[i], 2, str_length(input_d[i])))
    
    if (direction %in% c("U", "D")) {
      new_pos_start <- as.numeric(word(position[length(position)], 1, sep = ","))
    }
    
    if (direction %in% c("R", "L")) {
      new_pos_start <- as.numeric(word(position[length(position)], 2, sep = ","))
    }
    
    if (direction %in% c("U", "R")) new_pos_paces <- (new_pos_start + 1):(new_pos_start + paces)
    if (direction %in% c("D", "L")) new_pos_paces <- (new_pos_start - 1):(new_pos_start - paces)
    
    if (direction %in% c("R", "L")) {
      new_positions <- paste(word(position[length(position)], 1, sep = ","), new_pos_paces, sep = ",")
    }
    
    if (direction %in% c("U", "D")) {
      new_positions <- paste(new_pos_paces, word(position[length(position)], 2, sep = ","), sep = ",")
    }
    
    position <- c(position, new_positions)
  }
  position
}

# Get wire paths and find intersections
wire1_path <- path_gen(data[1, ])
wire2_path <- path_gen(data[2, ])
crossings <- intersect(wire1_path, wire2_path)
crossings %<>% .[2:length(.)]

# Find smallest distance not including initial point
crossings %>%
  as.data.frame() %>%
  janitor::clean_names() %>%
  mutate(
    dist = abs(as.numeric(word(x, 1, sep = ","))) + abs(as.numeric(word(x, 2, sep = ",")))
  ) %>%
  summarize(min(dist))
  


# PART 2 -------------------------------------------------------------------------------------------

# For each intersection point, figure out shortest number of steps for each wire to get there. Must
# subtract 1 from each match because wire_paths have 0,0 point.
total_distance <- (match(crossings, wire1_path) - 1) + (match(crossings, wire2_path) - 1)
min(total_distance)