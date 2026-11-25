source("src/utils.R")

# --- Day 16: The Floor Will Be Lava ---

input <- read_input("day_16")

parse_grid <- function(lines) {
  do.call(rbind, strsplit(lines, "", fixed = TRUE))
}

# Directions are 1 east, 2 south, 3 west, 4 north.
# `/` swaps east with north and south with west. `\` swaps east with south
# and west with north. A splitter hit on its flat side sends the beam both ways.
outgoing_dirs <- function(tile, dir) {
  if (tile == "." ||
      (tile == "-" && (dir == 1L || dir == 3L)) ||
      (tile == "|" && (dir == 2L || dir == 4L))) {
    return(dir)
  }
  if (tile == "-") return(c(1L, 3L))
  if (tile == "|") return(c(2L, 4L))
  if (tile == "/") return(5L - dir)
  if (tile == "\\") return(if (dir <= 2L) 3L - dir else 7L - dir)
  stop("Unknown tile: ", tile)
}

# A tile is energized when any beam passes through it. The same position and
# direction is never followed twice, so a loop cannot run forever.
energized_tiles <- function(grid, start_row = 1L, start_col = 1L, start_dir = 1L) {
  rows <- nrow(grid)
  cols <- ncol(grid)
  dr <- c(0L, 1L, 0L, -1L)
  dc <- c(1L, 0L, -1L, 0L)
  seen <- array(FALSE, c(rows, cols, 4L))

  sr <- integer(rows * cols * 4L)
  sc <- integer(rows * cols * 4L)
  sd <- integer(rows * cols * 4L)
  n <- 1L
  sr[[1]] <- start_row
  sc[[1]] <- start_col
  sd[[1]] <- start_dir
  seen[start_row, start_col, start_dir] <- TRUE

  while (n > 0L) {
    row <- sr[[n]]
    col <- sc[[n]]
    dir <- sd[[n]]
    n <- n - 1L
    for (next_dir in outgoing_dirs(grid[row, col], dir)) {
      next_row <- row + dr[[next_dir]]
      next_col <- col + dc[[next_dir]]
      if (next_row < 1L || next_row > rows || next_col < 1L || next_col > cols) next
      if (seen[next_row, next_col, next_dir]) next
      seen[next_row, next_col, next_dir] <- TRUE
      n <- n + 1L
      sr[[n]] <- next_row
      sc[[n]] <- next_col
      sd[[n]] <- next_dir
    }
  }

  sum(rowSums(seen, dims = 2L) > 0L)
}

part_one_solver <- function(lines) {
  energized_tiles(parse_grid(lines))
}

# The beam may enter any edge tile heading away from that edge. Corners can
# head in either of the two directions that leave the edge.
part_two_solver <- function(lines) {
  grid <- parse_grid(lines)
  rows <- nrow(grid)
  cols <- ncol(grid)
  best <- 0L
  for (col in seq_len(cols)) {
    best <- max(best, energized_tiles(grid, 1L, col, 2L))
    best <- max(best, energized_tiles(grid, rows, col, 4L))
  }
  for (row in seq_len(rows)) {
    best <- max(best, energized_tiles(grid, row, 1L, 1L))
    best <- max(best, energized_tiles(grid, row, cols, 3L))
  }
  best
}

example <- c(
  ".|...\\....",
  "|.-.\\.....",
  ".....|-...",
  "........|.",
  "..........",
  ".........\\",
  "..../.\\\\..",
  ".-.-/..|..",
  ".|....-|.\\",
  "..//.|...."
)
stopifnot(part_one_solver(example) == 46L)
stopifnot(energized_tiles(parse_grid(example), 1L, 4L, 2L) == 51L)
stopifnot(part_two_solver(example) == 51L)

res_01 <- part_one_solver(input)
res_02 <- part_two_solver(input)

cat(sprintf("Part 1 answer: %s\n", format(res_01, scientific = FALSE)))
cat(sprintf("Part 2 answer: %s\n", format(res_02, scientific = FALSE)))
