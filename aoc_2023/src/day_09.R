source("src/utils.R")

# --- Day 09: Mirage Maintenance ---

input <- read_input("day_09")

parse_histories <- function(lines) {
  lapply(strsplit(lines, "\\s+"), as.numeric)
}

# Difference rows end in a row of zeroes. A new value on a row is the edge
# beside it plus the new value of the row below, so the next value is the sum
# of every rightmost value and the previous value is the alternating sum of
# every leftmost value.
extrapolate <- function(history, forward = TRUE) {
  total <- 0
  sign <- 1
  row <- history
  repeat {
    edge <- if (forward) row[[length(row)]] else row[[1]]
    total <- total + sign * edge
    if (all(row == 0)) return(total)
    row <- diff(row)
    if (!forward) sign <- -sign
  }
}

part_one_solver <- function(lines) {
  sum(vapply(parse_histories(lines), extrapolate, numeric(1), forward = TRUE))
}

part_two_solver <- function(lines) {
  sum(vapply(parse_histories(lines), extrapolate, numeric(1), forward = FALSE))
}

example <- c(
  "0 3 6 9 12 15",
  "1 3 6 10 15 21",
  "10 13 16 21 30 45"
)
stopifnot(part_one_solver(example) == 114)
stopifnot(part_two_solver(example) == 2)

res_01 <- part_one_solver(input)
res_02 <- part_two_solver(input)

cat(sprintf("Part 1 answer: %s\n", format(res_01, scientific = FALSE)))
cat(sprintf("Part 2 answer: %s\n", format(res_02, scientific = FALSE)))
