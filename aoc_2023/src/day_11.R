source("src/utils.R")

# --- Day 11: Cosmic Expansion ---

input <- read_input("day_11")

# Galaxy coordinates are 1-indexed row, column pairs.
galaxy_positions <- function(lines) {
  grid <- do.call(rbind, strsplit(lines, ""))
  which(grid == "#", arr.ind = TRUE)
}

empty_lines <- function(lines) {
  grid <- do.call(rbind, strsplit(lines, ""))
  list(
    rows = which(rowSums(grid == "#") == 0),
    cols = which(colSums(grid == "#") == 0)
  )
}

# Each empty row or column is `expansion` lines wide. Crossing one adds
# `expansion - 1` steps on top of the Manhattan distance. Paths may pass
# through other galaxies. Distances stay numeric so a million-fold expansion
# does not overflow R's 32-bit integers.
pair_distance <- function(a, b, empty, expansion) {
  extra <- expansion - 1
  row_span <- sort(c(a[[1]], b[[1]]))
  col_span <- sort(c(a[[2]], b[[2]]))
  rows_crossed <- sum(empty$rows > row_span[[1]] & empty$rows < row_span[[2]])
  cols_crossed <- sum(empty$cols > col_span[[1]] & empty$cols < col_span[[2]])
  (row_span[[2]] - row_span[[1]]) + (col_span[[2]] - col_span[[1]]) +
    extra * (rows_crossed + cols_crossed)
}

sum_paths <- function(lines, expansion) {
  galaxies <- galaxy_positions(lines)
  empty <- empty_lines(lines)
  n <- nrow(galaxies)
  total <- 0
  if (n < 2L) return(total)
  for (i in seq_len(n - 1L)) {
    for (j in (i + 1L):n) {
      total <- total + pair_distance(galaxies[i, ], galaxies[j, ], empty, expansion)
    }
  }
  total
}

part_one_solver <- function(lines) sum_paths(lines, 2)

part_two_solver <- function(lines) sum_paths(lines, 1000000)

example <- c(
  "...#......",
  ".......#..",
  "#.........",
  "..........",
  "......#...",
  ".#........",
  ".........#",
  "..........",
  ".......#..",
  "#...#....."
)
stopifnot(part_one_solver(example) == 374)
stopifnot(sum_paths(example, 10) == 1030)
stopifnot(sum_paths(example, 100) == 8410)

res_01 <- part_one_solver(input)
res_02 <- part_two_solver(input)

cat(sprintf("Part 1 answer: %s\n", format(res_01, scientific = FALSE)))
cat(sprintf("Part 2 answer: %s\n", format(res_02, scientific = FALSE)))
