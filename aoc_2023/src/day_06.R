source("src/utils.R")

# --- Day 06: Wait For It ---

input <- read_input("day_06")

# Each line is a label followed by one integer per race
parse_races <- function(lines) {
  numbers <- function(line) {
    as.numeric(strsplit(sub("^\\w+:\\s*", "", line), "\\s+")[[1]])
  }
  list(time = numbers(lines[[1]]), distance = numbers(lines[[2]]))
}

# Holding the button for `hold` ms sets speed to `hold` and leaves `time - hold` to travel.
# Distance beats the record when hold * (time - hold) > distance, i.e. when hold sits
# strictly between the roots of hold^2 - time * hold + distance = 0.
ways_to_win <- function(time, distance) {
  root <- sqrt(time * time - 4 * distance)
  low <- (time - root) / 2
  high <- (time + root) / 2
  first <- floor(low) + 1
  last <- ceiling(high) - 1

  beats <- function(hold) hold * (time - hold) > distance
  # Correct a root that floating point nudged across an integer.
  while (first <= last && !beats(first)) first <- first + 1
  while (last >= first && !beats(last)) last <- last - 1
  while (first > 0 && beats(first - 1)) first <- first - 1
  while (last < time && beats(last + 1)) last <- last + 1

  last - first + 1
}

part_one_solver <- function(lines) {
  races <- parse_races(lines)
  ways <- mapply(ways_to_win, races$time, races$distance)
  prod(ways)
}

# Part 2 ignores the spaces, so each line is a single number.
part_two_solver <- function(lines) {
  number <- function(line) as.numeric(gsub("[^0-9]", "", line))
  ways_to_win(number(lines[[1]]), number(lines[[2]]))
}

res_01 <- part_one_solver(input)
res_02 <- part_two_solver(input)

# Output results
cat(sprintf("Part 1 answer: %s\n", format(res_01, scientific = FALSE)))
cat(sprintf("Part 2 answer: %s\n", format(res_02, scientific = FALSE)))
