# SET-UP -------------------------------------------------------------------------------------------

# Clear workspace
rm(list = ls())

# Load core packages
pacman::p_load(dplyr, tidyr, stringr, magrittr, here, readr, purrr)

# Load puzzle input
data <- scan(here::here("data/day7_data.txt"), sep = ",")



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


# Write a function that walks through a code with a given input vector creates the output and then
# walks until another input is needed.
code_walker <- function(current_data = NULL, input_chain = NULL, current_index = NULL, 
                        current_input = NULL) {
  stop_overall <- F
  stop_step <- F
  data <- current_data
  i <- current_index
  j <- current_input
  while (!stop_overall & !stop_step) {
    if (data[i] == 99 | i > length(data)) stop_overall <- T
    
    else {
      codes <- code_finder(data[i])
      param1 <- ifelse(codes$paramcodes[1] == 0, data[data[i + 1] + 1], data[i + 1])
      param2 <- ifelse(codes$paramcodes[2] == 0, data[data[i + 2] + 1], data[i + 2])
      param3 <- data[i + 3]
      
      if (codes$opcode == 1) data[data[i + 3] + 1] <- param1 + param2
      if (codes$opcode == 2) data[data[i + 3] + 1] <- param1 * param2
      if (codes$opcode == 3 & is.na(input_chain[j])) stop_step <- T
      if (codes$opcode == 3 & !is.na(input_chain[j])) {
        data[data[i + 1] + 1] <- input_chain[j]
        j <- j + 1
      }
      if (codes$opcode == 4) output <- param1
      if (codes$opcode == 5) i <- ifelse(param1 != 0, param2 + 1, i + 3)
      if (codes$opcode == 6) i <- ifelse(param1 == 0, param2 + 1, i + 3)
      if (codes$opcode == 7) data[param3 + 1] <- ifelse(param1 < param2, 1, 0)
      if (codes$opcode == 8) data[param3 + 1] <- ifelse(param1 == param2, 1, 0)
      
      if (codes$opcode %in% c(1, 2, 7, 8) & !stop_step) i <- i + 4
      if (codes$opcode %in% 3:4 & !stop_step) i <- i + 2
    }
  }
  
  list(
    "data" = data, 
    "input" = input_chain, 
    "data_index" = i, 
    "input_index" = j, 
    "output" = output, 
    "stop" = stop_overall
  )
  
}

# Write function that walks through code but also tracks input vectors and location indices
output_producer <- function(init_data = NULL, phase_setting = NULL) {
  
  # Make a list element for each phase-setting. This creates lists of lists with first element the
  # data chain, second element the input chain, and third element is te index indicating where
  # in the chain we currently are. The fourth element is a placehodler for outputs.
  data_list <- vector(mode = "list", length = length(phase_setting))
  for (i in 1:length(phase_setting)) {
    data_list[[i]] <- vector(mode = "list", length = 5)
    data_list[[i]][[1]] <- init_data
    data_list[[i]][[2]] <- c(phase_setting[i])
    data_list[[i]][[3]] <- 1
    data_list[[i]][[4]] <- 1
    data_list[[i]][[6]] <- F
    names(data_list[[i]]) <- c("data", "input", "data_index", "input_index", "output", "stop")
  }
  
  # Add initial 0 signal to first amplifier
  data_list[[1]][[2]] <- c(data_list[[1]][[2]], 0)
  
  # Run code_walker function on sublists until halt
  t <- 1
  while (!data_list[[5]]$stop) {
    result <- code_walker(
      current_data = data_list[[t]]$data,
      input_chain = data_list[[t]]$input, 
      current_index = data_list[[t]]$data_index, 
      current_input = data_list[[t]]$input_index
    )
    if (t < 5) {
      data_list[[t]] <- result
      data_list[[t + 1]]$input <- c(data_list[[t + 1]]$input, result$output)
      t <- t + 1
    } else {
      data_list[[t]] <- result
      data_list[[1]]$input <- c(data_list[[1]]$input, result$output)
      t <- 1
    }
    
  }
  
  data_list[[5]]$output
}



# PART 1 -------------------------------------------------------------------------------------------

# Get vector of all possible permutations from arrangements package and then output function
arrangements::permutations(0:4, 5) %>%
  split(row(.)) %>%
  map_dbl(~ output_producer(data, .x)) %>%
  max()



# PART 2 -------------------------------------------------------------------------------------------

# Get vector of all possible permutations from arrangements package and then output function
arrangements::permutations(5:9, 5) %>%
  split(row(.)) %>%
  map_dbl(~ output_producer(data, .x)) %>%
  max()