source("src/utils.R")

# --- Day 02: Cube Conundrum ---

input <- read_input("day_02")

# Parse a single game line into structured data
parse_game <- function(line) {
  # Extract game ID
  game_id <- as.numeric(sub("Game (\\d+):.*", "\\1", line))
  
  # Extract the game data (after the colon)
  game_data <- sub("Game \\d+: ", "", line)
  
  # Split by semicolons to get each reveal
  reveals <- strsplit(game_data, "; ")[[1]]
  
  # Parse each reveal
  all_cubes <- lapply(reveals, function(reveal) {
    # Split by comma to get individual color counts
    cube_counts <- strsplit(reveal, ", ")[[1]]
    
    # Initialize counts
    cubes <- list(red = 0, green = 0, blue = 0)
    
    # Parse each color count
    for (count_str in cube_counts) {
      parts <- strsplit(trimws(count_str), " ")[[1]]
      count <- as.numeric(parts[1])
      color <- parts[2]
      cubes[[color]] <- count
    }
    
    cubes
  })
  
  list(id = game_id, reveals = all_cubes)
}

# Parse all games
games <- lapply(input, parse_game)

# Part 1: Which games are possible with 12 red, 13 green, 14 blue?
part_one_solver <- function(games) {
  max_cubes <- list(red = 12, green = 13, blue = 14)
  
  possible_game_ids <- sapply(games, function(game) {
    # Check if all reveals in this game are possible
    is_possible <- all(sapply(game$reveals, function(reveal) {
      reveal$red <= max_cubes$red &&
        reveal$green <= max_cubes$green &&
        reveal$blue <= max_cubes$blue
    }))
    
    if (is_possible) game$id else 0
  })
  
  sum(possible_game_ids)
}

res_01 <- part_one_solver(games)

# Part 2: Find minimum cubes needed for each game, calculate power, sum them
part_two_solver <- function(games) {
  powers <- sapply(games, function(game) {
    # Find minimum cubes needed (max of each color across all reveals)
    min_red <- max(sapply(game$reveals, function(r) r$red))
    min_green <- max(sapply(game$reveals, function(r) r$green))
    min_blue <- max(sapply(game$reveals, function(r) r$blue))
    
    # Calculate power (product of minimums)
    min_red * min_green * min_blue
  })
  
  sum(powers)
}

res_02 <- part_two_solver(games)

# Output results
cat(sprintf("Part 1 answer: %s\n", res_01))
cat(sprintf("Part 2 answer: %s\n", res_02))

