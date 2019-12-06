# SET-UP -------------------------------------------------------------------------------------------

# Clear workspace
rm(list = ls())

# Load core packages
pacman::p_load(dplyr, tidyr, stringr, magrittr, here, readr, purrr)

# Load puzzle input and swap out ) to avoid reg ex issues
data <- 
  read_csv(here::here("data/day6_data.txt"), col_names = F) %>%
  pull(1)
data <- str_replace(data, "\\)", "@")

data <- c(
"COM)B",
"B)C",
"C)D",
"D)E",
"E)F",
"B)G",
"G)H",
"D)I",
"E)J",
"J)K",
"K)L",
"K)YOU",
"I)SAN"
 )


# PART 1 -------------------------------------------------------------------------------------------

# Because every object can only orbit 1 other, simply start from the end of the chain and find all
# connections

# Split into two vectors to make code easier to follow
orbiter <- word(data, 2, sep = "@")
orbited <- word(data, 1, sep = "@")

# For each step in the chain count number of direct and indirect orbits and add to counters
direct_orbits <- length(data)
indirect_orbits <- 0
for (i in 1:length(data)) {
  step_orbited <- orbited[orbiter == orbited[i]]
  while (length(step_orbited) > 0) {
    indirect_orbits <- indirect_orbits + 1
    step_orbited <- orbited[orbiter == step_orbited]
  }
}

direct_orbits + indirect_orbits



# PART 2 -------------------------------------------------------------------------------------------

# Mapping out all possible paths from YOU until SAN is reached
current_paths <- list(c("YOU", orbited[orbiter == "YOU"]))
stop <- F
while (!stop) {
  
  current_steps <-
    map(
      .x = current_paths,
      .f = ~ .x[length(.x)]
    )
  
  current_options <- current_steps %>%
    map(~ data[str_detect(data, paste0(word(.x, 1, sep = "@"), "|", word(.x, 2, sep = "@")))]) %>%
    map(~ unique(c(word(.x, 1, sep = "@"), word(.x, 2, sep = "@")))) %>%
    map2(
      .y = current_paths,
      .f = ~ .x[!.x %in% .y]
    )
  
  current_paths %<>%
    map2(
      .y = current_options,
      .f = 
        ~ matrix(rep(.x, length(.y)), nrow = length(.y), byrow = T) %>%
        cbind(.y) %>%
        as.data.frame(stringsAsFactors = F)
    ) %>%
    bind_rows()
  
  # Testing if SAN is in a move. If yes, stop and return output. If no then drop rows with no new
  # moves and prepare for next loop.
  if (max(current_paths$.y == "SAN") == 1) {
    stop <- T
    output <- current_paths %>% filter(.y =="SAN")
    print(length(names(current_paths)) - 3)
  } else {
    non_y_names <- names(current_paths)[names(current_paths) != ".y"]
    current_paths %<>%
      select(non_y_names, .y) %>%
      filter(!is.na(.y)) %>%
      split(1:nrow(.)) %>%
      map(~ unname(unlist(.x)))
  }

}