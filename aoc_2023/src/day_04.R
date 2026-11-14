source("src/utils.R")

# --- Day 04: Scratchcards ---

input <- read_input("day_04")

# Winning numbers and the numbers held on each card
parse_cards <- function(lines) {
  lapply(lines, function(line) {
    body <- sub("^Card\\s+\\d+:\\s*", "", line)
    parts <- strsplit(body, "\\|")[[1]]
    numbers <- function(text) {
      as.integer(strsplit(trimws(text), "\\s+")[[1]])
    }
    list(winning = numbers(parts[[1]]), held = numbers(parts[[2]]))
  })
}

# How many held numbers appear in the winning list
match_counts <- function(cards) {
  vapply(cards, function(card) {
    length(intersect(card$held, card$winning))
  }, integer(1))
}

part_one_solver <- function(lines) {
  counts <- match_counts(parse_cards(lines))
  points <- ifelse(counts == 0L, 0, 2^(counts - 1L))
  sum(points)
}

# Each match wins a copy of one of the following cards
part_two_solver <- function(lines) {
  counts <- match_counts(parse_cards(lines))
  n <- length(counts)
  copies <- rep(1, n)

  for (i in seq_len(n)) {
    matches <- counts[[i]]
    if (matches == 0L || i == n) next
    last <- min(n, i + matches)
    copies[(i + 1L):last] <- copies[(i + 1L):last] + copies[[i]]
  }

  sum(copies)
}

res_01 <- part_one_solver(input)
res_02 <- part_two_solver(input)

# Output results
cat(sprintf("Part 1 answer: %s\n", res_01))
cat(sprintf("Part 2 answer: %s\n", res_02))
