# SET-UP -------------------------------------------------------------------------------------------

# Clear workspace
rm(list = ls())

# Load core packages
pacman::p_load(dplyr, tidyr, stringr, forcats, magrittr, here, readxl, readr, purrr)



# PART 1 -------------------------------------------------------------------------------------------

# Return values with two adjacent numbers in range
double_vals <- paste(347312:805915)[str_detect(paste(347312:805915), "00|11|22|33|44|55|66|77|88|99")]

# Separate each number in the sequence into separate columns. The first with the first 5 digits and the
# second with the last 5 digits. and then subtracting and looking for rows where no difference is 
# less tahn 0. Reading the data as a fixed width file.
final_data <- 
  read.fwf(
    file = textConnection(double_vals, open = "r"),
    widths = c(1, 1, 1, 1, 1, 1)
  ) 

final_data_part1 <-
  pmap_dbl(
    .l = final_data[, 2:6] - final_data[, 1:5],
    .f = min
  )

sum(final_data_part1 >= 0)
 


# PART 2 -------------------------------------------------------------------------------------------

# Starting with all values with at least some double values, drop cases where there is at least one
# instance of a double value without a triple. Then repeat steps from Part 1.
final_data_part2 <- final_data %>%
  filter(
    (V1 == V2 & V2 != V3) |
    (V2 == V3 & V1 != V2 & V3 != V4) |
    (V3 == V4 & V2 != V3 & V4 != V5) |
    (V4 == V5 & V3 != V4 & V5 != V6) |
    (V5 == V6 & V4 != V5)
  )

final_data_part2 <-
  pmap_dbl(
    .l = final_data_part2[, 2:6] - final_data_part2[, 1:5],
    .f = min
  )

sum(final_data_part2 >= 0)