source("src/utils.R")

# --- Day 14: Parabolic Reflector Dish ---

input <- read_input("day_14")

parse_grid <- function(lines) {
  do.call(rbind, strsplit(lines, "", fixed = TRUE))
}

# Rounded rocks roll north until they hit a cube or another rounded rock.
tilt_north <- function(grid) {
  rows <- nrow(grid)
  cols <- ncol(grid)
  for (col in seq_len(cols)) {
    next_row <- 1L
    for (row in seq_len(rows)) {
      cell <- grid[row, col]
      if (cell == "#") {
        next_row <- row + 1L
      } else if (cell == "O") {
        if (row != next_row) {
          grid[next_row, col] <- "O"
          grid[row, col] <- "."
        }
        next_row <- next_row + 1L
      }
    }
  }
  grid
}

tilt_south <- function(grid) {
  rows <- nrow(grid)
  cols <- ncol(grid)
  for (col in seq_len(cols)) {
    next_row <- rows
    for (row in rows:1L) {
      cell <- grid[row, col]
      if (cell == "#") {
        next_row <- row - 1L
      } else if (cell == "O") {
        if (row != next_row) {
          grid[next_row, col] <- "O"
          grid[row, col] <- "."
        }
        next_row <- next_row - 1L
      }
    }
  }
  grid
}

tilt_west <- function(grid) {
  rows <- nrow(grid)
  cols <- ncol(grid)
  for (row in seq_len(rows)) {
    next_col <- 1L
    for (col in seq_len(cols)) {
      cell <- grid[row, col]
      if (cell == "#") {
        next_col <- col + 1L
      } else if (cell == "O") {
        if (col != next_col) {
          grid[row, next_col] <- "O"
          grid[row, col] <- "."
        }
        next_col <- next_col + 1L
      }
    }
  }
  grid
}

tilt_east <- function(grid) {
  rows <- nrow(grid)
  cols <- ncol(grid)
  for (row in seq_len(rows)) {
    next_col <- cols
    for (col in cols:1L) {
      cell <- grid[row, col]
      if (cell == "#") {
        next_col <- col - 1L
      } else if (cell == "O") {
        if (col != next_col) {
          grid[row, next_col] <- "O"
          grid[row, col] <- "."
        }
        next_col <- next_col - 1L
      }
    }
  }
  grid
}

# One spin rolls the rocks north, then west, then south, then east.
spin_cycle <- function(grid) {
  tilt_east(tilt_south(tilt_west(tilt_north(grid))))
}

grid_lines <- function(grid) {
  vapply(seq_len(nrow(grid)), function(row) paste(grid[row, ], collapse = ""), character(1))
}

# A rounded rock loads the north beams by the number of rows from it to the
# south edge, including its own row. Cubes do not contribute.
north_load <- function(grid) {
  rows <- nrow(grid)
  sum((rows:1L) * rowSums(grid == "O"))
}

# The platform repeats, so the state after `cycles` is the same as the state
# that many steps into the first repeated loop.
load_after_cycles <- function(grid, cycles) {
  keys <- character()
  loads <- numeric()
  for (i in seq_len(cycles)) {
    grid <- spin_cycle(grid)
    key <- paste(grid_lines(grid), collapse = "\n")
    first <- match(key, keys)
    if (!is.na(first)) {
      period <- i - first
      target <- first + (cycles - first) %% period
      return(loads[[target]])
    }
    keys[[i]] <- key
    loads[[i]] <- north_load(grid)
  }
  north_load(grid)
}

part_one_solver <- function(lines) {
  north_load(tilt_north(parse_grid(lines)))
}

part_two_solver <- function(lines, cycles = 1000000000) {
  load_after_cycles(parse_grid(lines), cycles)
}

example <- c(
  "O....#....",
  "O.OO#....#",
  ".....##...",
  "OO.#O....O",
  ".O.....O#.",
  "O.#..O.#.#",
  "..O..#O..O",
  ".......O..",
  "#....###..",
  "#OO..#...."
)
after_one <- c(
  ".....#....",
  "....#...O#",
  "...OO##...",
  ".OO#......",
  ".....OOO#.",
  ".O#...O#.#",
  "....O#....",
  "......OOOO",
  "#...O###..",
  "#..OO#...."
)
after_two <- c(
  ".....#....",
  "....#...O#",
  ".....##...",
  "..O#......",
  ".....OOO#.",
  ".O#...O#.#",
  "....O#...O",
  ".......OOO",
  "#..OO###..",
  "#.OOO#...O"
)
after_three <- c(
  ".....#....",
  "....#...O#",
  ".....##...",
  "..O#......",
  ".....OOO#.",
  ".O#...O#.#",
  "....O#...O",
  ".......OOO",
  "#...O###.O",
  "#.OOO#...O"
)
example_grid <- parse_grid(example)
stopifnot(part_one_solver(example) == 136)
stopifnot(identical(grid_lines(spin_cycle(example_grid)), after_one))
stopifnot(identical(grid_lines(spin_cycle(spin_cycle(example_grid))), after_two))
stopifnot(identical(grid_lines(spin_cycle(spin_cycle(spin_cycle(example_grid)))), after_three))
stopifnot(part_two_solver(example) == 64)

res_01 <- part_one_solver(input)
res_02 <- part_two_solver(input)

cat(sprintf("Part 1 answer: %s\n", format(res_01, scientific = FALSE)))
cat(sprintf("Part 2 answer: %s\n", format(res_02, scientific = FALSE)))
