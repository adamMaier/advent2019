# SET-UP -------------------------------------------------------------------------------------------

# Clear workspace
rm(list = ls())

# Load core packages
pacman::p_load(dplyr, tidyr, stringr, magrittr, here, readr, purrr)

# Load puzzle input
data <- scan(here::here("data/day5_data.txt"), sep = ",")



# BOTH PARTS ---------------------------------------------------------------------------------------

# Write function to obtain opcode and paramater codes from a value
code_finder <- function(value = NULL) {
  
  # Determine opcode with last two digits of value
  opcode <- as.numeric(str_sub(value, -2))
  
  # Get parameter modes. If opcode is 1, 2, 7, 8, then 3 parameters are needed; if 3 or 4, only 1.
  # And if 5 or 6, then 2 paramaters
  if (opcode %in% c(1, 2, 7, 8)) {
    paramcodes <- str_pad(value, side = "left", width = 5, pad = "0")
    paramcodes <- str_sub(paramcodes, 1, -3)
    paramcodes <- str_split(paramcodes, pattern = "", simplify = T)
    paramcodes <- as.numeric(paramcodes)
    paramcodes <- rev(paramcodes)
  }
  
  if (opcode %in% 3:4) {
    paramcodes <- str_pad(value, side = "left", width = 3, pad = "0")
    paramcodes <- str_sub(paramcodes, 1, -3)
    paramcodes <- str_split(paramcodes, pattern = "", simplify = T)
    paramcodes <- as.numeric(paramcodes)
    paramcodes <- rev(paramcodes)
  }
  
  if (opcode %in% 5:6) {
    paramcodes <- str_pad(value, side = "left", width = 4, pad = "0")
    paramcodes <- str_sub(paramcodes, 1, -3)
    paramcodes <- str_split(paramcodes, pattern = "", simplify = T)
    paramcodes <- as.numeric(paramcodes)
    paramcodes <- rev(paramcodes)
  }
  
  list("opcode" = opcode, "paramcodes" = paramcodes)
  
}



# PART 1 -------------------------------------------------------------------------------------------

# Write function that takes input and walks through codes
code_walker_p1 <- function(init_data = NULL, init_input = NULL) {
  
  # Create empty output vector and get data ready
  output <- c()
  data <- init_data
  input <- init_input

  # Run while loop until program ends
  stop <- F
  i <- 1
  while (!stop) {
    if (data[i] == 99 | i > length(data)) stop <- T
    else {
      codes <- code_finder(data[i])
      param1 <- ifelse(codes$paramcodes[1] == 0, data[data[i + 1] + 1], data[i + 1])
      param2 <- ifelse(codes$paramcodes[2] == 0, data[data[i + 2] + 1], data[i + 2])
      if (codes$opcode == 1) data[data[i + 3] + 1] <- param1 + param2
      if (codes$opcode == 2) data[data[i + 3] + 1] <- param1 * param2
      if (codes$opcode == 3) data[data[i + 1] + 1] <- input
      if (codes$opcode == 4) output <- c(output, param1)
      i <- ifelse(codes$opcode <= 2,  i + 4, i + 2)
    }
  }
  output
}

# Get result
code_walker_p1(data, 1)



# PART 2 -------------------------------------------------------------------------------------------

# Write function that takes input and walks through codes
code_walker_p2 <- function(init_data = NULL, init_input = NULL) {
  
  # Create empty output vector and get data ready
  output <- c()
  data <- init_data
  input <- init_input
  
  # Run while loop until program ends
  stop <- F
  i <- 1
  while (!stop) {
    
    if (data[i] == 99 | i > length(data)) stop <- T
    
    else {
      codes <- code_finder(data[i])
      param1 <- ifelse(codes$paramcodes[1] == 0, data[data[i + 1] + 1], data[i + 1])
      param2 <- ifelse(codes$paramcodes[2] == 0, data[data[i + 2] + 1], data[i + 2])
      param3 <- data[i + 3]
  
      if (codes$opcode == 1) data[data[i + 3] + 1] <- param1 + param2
      if (codes$opcode == 2) data[data[i + 3] + 1] <- param1 * param2
      if (codes$opcode == 3) data[data[i + 1] + 1] <- input
      if (codes$opcode == 4) output <- c(output, param1)
      if (codes$opcode == 5) i <- ifelse(param1 != 0, param2 + 1, i + 3)
      if (codes$opcode == 6) i <- ifelse(param1 == 0, param2 + 1, i + 3)
      if (codes$opcode == 7) data[param3 + 1] <- ifelse(param1 < param2, 1, 0)
      if (codes$opcode == 8) data[param3 + 1] <- ifelse(param1 == param2, 1, 0)
      
      if (codes$opcode %in% c(1, 2, 7, 8)) i <- i + 4
      if (codes$opcode %in% 3:4) i <- i + 2
    }
  }
  output
}

code_walker_p2(data, 5)