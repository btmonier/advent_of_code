source("src/utils.R")

# --- Day 07: Camel Cards ---

input <- read_input("day_07")

# Card strength, low to high. In part 1, J sits between T and Q.
card_rank <- c(
  "2" = 2, "3" = 3, "4" = 4, "5" = 5, "6" = 6, "7" = 7, "8" = 8, "9" = 9,
  "T" = 10, "J" = 11, "Q" = 12, "K" = 13, "A" = 14
)

# Part 2: J is a joker, and the weakest card when comparing individual cards.
card_rank_joker <- replace(card_rank, "J", 1)

# Type strength from a descending vector of label counts.
type_from_counts <- function(counts) {
  if (identical(counts, 5L)) return(7L)            # five of a kind
  if (identical(counts, c(4L, 1L))) return(6L)     # four of a kind
  if (identical(counts, c(3L, 2L))) return(5L)     # full house
  if (identical(counts, c(3L, 1L, 1L))) return(4L) # three of a kind
  if (identical(counts, c(2L, 2L, 1L))) return(3L) # two pair
  if (identical(counts, c(2L, 1L, 1L, 1L))) return(2L) # one pair
  1L # high card
}

# Type strength, low to high, so a plain ascending sort puts the weakest hand first.
# With jokers, every J is assigned to the most common other label, which is the
# assignment that produces the strongest type. A hand of five jokers is five of a kind.
hand_type <- function(cards, jokers = FALSE) {
  wild <- if (jokers) as.integer(sum(cards == "J")) else 0L
  if (jokers) cards <- cards[cards != "J"]
  if (length(cards) == 0) return(7L)

  counts <- sort(as.integer(table(cards)), decreasing = TRUE)
  counts[[1]] <- counts[[1]] + wild
  type_from_counts(counts)
}

parse_hands <- function(lines) {
  parts <- strsplit(lines, "\\s+")
  data.frame(
    cards = vapply(parts, `[[`, character(1), 1),
    bid = vapply(parts, function(p) as.numeric(p[[2]]), numeric(1)),
    stringsAsFactors = FALSE
  )
}

# Rank is 1 for the weakest hand. Winnings are bid multiplied by rank.
# Tie breaks still compare the cards as written, using `strength`.
total_winnings <- function(lines, jokers, strength) {
  hands <- parse_hands(lines)
  cards <- strsplit(hands$cards, "")
  type <- vapply(cards, hand_type, integer(1), jokers = jokers)
  ranks <- lapply(cards, function(c) unname(strength[c]))

  order_keys <- do.call(rbind, lapply(seq_along(type), function(i) {
    c(type[[i]], ranks[[i]])
  }))
  ranked <- order(
    order_keys[, 1], order_keys[, 2], order_keys[, 3],
    order_keys[, 4], order_keys[, 5], order_keys[, 6]
  )
  sum(hands$bid[ranked] * seq_along(ranked))
}

part_one_solver <- function(lines) total_winnings(lines, jokers = FALSE, card_rank)
part_two_solver <- function(lines) total_winnings(lines, jokers = TRUE, card_rank_joker)

example <- c(
  "32T3K 765",
  "T55J5 684",
  "KK677 28",
  "KTJJT 220",
  "QQQJA 483"
)
stopifnot(part_one_solver(example) == 6440)
stopifnot(part_two_solver(example) == 5905)

res_01 <- part_one_solver(input)
res_02 <- part_two_solver(input)

cat(sprintf("Part 1 answer: %s\n", format(res_01, scientific = FALSE)))
cat(sprintf("Part 2 answer: %s\n", format(res_02, scientific = FALSE)))
