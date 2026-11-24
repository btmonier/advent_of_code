source("src/utils.R")

# --- Day 12: Hot Springs ---

input <- read_input("day_12")

parse_record <- function(line) {
  parts <- strsplit(line, " ", fixed = TRUE)[[1]]
  list(
    springs = strsplit(parts[[1]], "", fixed = TRUE)[[1]],
    groups = as.integer(strsplit(parts[[2]], ",", fixed = TRUE)[[1]])
  )
}

# A damaged run fits at `start` when every spring in it is damaged or unknown,
# and the spring just after the run is not damaged. Groups must be separated.
group_fits <- function(springs, start, run) {
  n <- length(springs)
  end <- start + run - 1L
  if (end > n) return(FALSE)
  if (any(springs[start:end] == ".")) return(FALSE)
  end == n || springs[[end + 1L]] != "#"
}

# Ways to place `groups[gi:...]` on `springs[i:...]`. An operational spring is
# skipped. A damaged spring must open the next group. An unknown spring tries both.
count_arrangements <- function(springs, groups) {
  n <- length(springs)
  gcount <- length(groups)
  memo <- matrix(NA_real_, n + 1L, gcount + 1L)

  ways <- function(i, gi) {
    cached <- memo[i, gi]
    if (!is.na(cached)) return(cached)

    if (gi > gcount) {
      result <- if (i > n || all(springs[i:n] != "#")) 1 else 0
    } else if (i > n) {
      result <- 0
    } else {
      result <- 0
      spring <- springs[[i]]
      if (spring != "#") result <- result + ways(i + 1L, gi)
      if (spring != "." && group_fits(springs, i, groups[[gi]])) {
        result <- result + ways(min(i + groups[[gi]] + 1L, n + 1L), gi + 1L)
      }
    }

    memo[i, gi] <<- result
    result
  }

  ways(1L, 1L)
}

# Folded records repeat the springs five times, joined by unknown springs, and
# repeat the group sizes five times.
unfold_record <- function(record, times) {
  pattern <- paste(rep(paste(record$springs, collapse = ""), times), collapse = "?")
  list(
    springs = strsplit(pattern, "", fixed = TRUE)[[1]],
    groups = rep(record$groups, times)
  )
}

count_line <- function(line, times = 1L) {
  record <- parse_record(line)
  if (times > 1L) record <- unfold_record(record, times)
  count_arrangements(record$springs, record$groups)
}

part_one_solver <- function(lines) {
  sum(vapply(lines, count_line, numeric(1), times = 1L))
}

part_two_solver <- function(lines) {
  sum(vapply(lines, count_line, numeric(1), times = 5L))
}

example <- c(
  "???.### 1,1,3",
  ".??..??...?##. 1,1,3",
  "?#?#?#?#?#?#?#? 1,3,1,6",
  "????.#...#... 4,1,1",
  "????.######..#####. 1,6,5",
  "?###???????? 3,2,1"
)
known <- c(
  "#.#.### 1,1,3",
  ".#...#....###. 1,1,3",
  ".#.###.#.###### 1,3,1,6",
  "####.#...#... 4,1,1",
  "#....######..#####. 1,6,5",
  ".###.##....# 3,2,1"
)
stopifnot(part_one_solver(example[1]) == 1)
stopifnot(part_one_solver(example[2]) == 4)
stopifnot(part_one_solver(example[3]) == 1)
stopifnot(part_one_solver(example[4]) == 1)
stopifnot(part_one_solver(example[5]) == 4)
stopifnot(part_one_solver(example[6]) == 10)
stopifnot(part_one_solver(example) == 21)
stopifnot(part_one_solver(known) == length(known))
stopifnot(part_two_solver(example[1]) == 1)
stopifnot(part_two_solver(example[2]) == 16384)
stopifnot(part_two_solver(example[3]) == 1)
stopifnot(part_two_solver(example[4]) == 16)
stopifnot(part_two_solver(example[5]) == 2500)
stopifnot(part_two_solver(example[6]) == 506250)
stopifnot(part_two_solver(example) == 525152)

res_01 <- part_one_solver(input)
res_02 <- part_two_solver(input)

cat(sprintf("Part 1 answer: %s\n", format(res_01, scientific = FALSE)))
cat(sprintf("Part 2 answer: %s\n", format(res_02, scientific = FALSE)))
