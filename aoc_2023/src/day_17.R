source("src/utils.R")

# --- Day 17: Clumsy Crucible ---

input <- read_input("day_17")

parse_grid <- function(lines) {
  matrix(
    as.integer(unlist(strsplit(lines, "", fixed = TRUE))),
    nrow = length(lines),
    byrow = TRUE
  )
}

# Least heat lost walking from the top-left to the bottom-right. The start
# block is free. The crucible may not reverse, and once it starts in a
# direction it must walk between `min_straight` and `max_straight` blocks
# before it can turn or stop.
#
# Each step adds only a few heat points, so states are stored in a bucket per
# cost and expanded from cheapest to most expensive. That replaces the binary
# heap, whose swaps dominated the running time.
least_heat <- function(grid, min_straight = 1L, max_straight = 3L) {
  rows <- nrow(grid)
  cols <- ncol(grid)
  # 1 north, 2 east, 3 south, 4 west.
  dr <- c(-1L, 0L, 1L, 0L)
  dc <- c(0L, 1L, 0L, -1L)
  left <- c(4L, 1L, 2L, 3L)
  right <- c(2L, 3L, 4L, 1L)
  huge <- .Machine$integer.max
  # State index: streak, then direction, column, and row.
  dist <- rep(huge, rows * cols * 4L * max_straight)

  # A state can be queued again when a cheaper path reaches it.
  nodes <- integer(length(dist) * 4L)
  nxt <- integer(length(dist) * 4L)
  ncount <- 0L
  # Heat per step is a single digit, so ten buckets cover every cost that can
  # still be reached from the cost currently being expanded.
  head <- integer(10L)
  tail <- integer(10L)
  max_cost <- 0L

  for (d in 1:4) {
    r <- 1L + dr[[d]]
    c <- 1L + dc[[d]]
    if (r < 1L || r > rows || c < 1L || c > cols) next
    ncost <- grid[[r + (c - 1L) * rows]]
    ndi <- 1L + max_straight * ((d - 1L) + 4L * ((c - 1L) + cols * (r - 1L)))
    dist[[ndi]] <- ncost
    ncount <- ncount + 1L
    nodes[[ncount]] <- ndi
    nxt[[ncount]] <- 0L
    bucket <- ncost %% 10L + 1L
    if (head[[bucket]] == 0L) head[[bucket]] <- ncount else nxt[[tail[[bucket]]]] <- ncount
    tail[[bucket]] <- ncount
    if (ncost > max_cost) max_cost <- ncost
  }

  cost <- 0L
  while (cost <= max_cost) {
    bucket <- cost %% 10L + 1L
    idx <- head[[bucket]]
    if (idx == 0L) {
      cost <- cost + 1L
      next
    }
    head[[bucket]] <- nxt[[idx]]
    di <- nodes[[idx]]
    if (cost > dist[[di]]) next

    s <- (di - 1L) %% max_straight + 1L
    tmp <- (di - 1L) %/% max_straight
    d <- tmp %% 4L + 1L
    tmp <- tmp %/% 4L
    c <- tmp %% cols + 1L
    r <- tmp %/% cols + 1L
    if (r == rows && c == cols && s >= min_straight) return(cost)

    if (s < max_straight) {
      nr <- r + dr[[d]]
      nc <- c + dc[[d]]
      if (nr >= 1L && nr <= rows && nc >= 1L && nc <= cols) {
        ncost <- cost + grid[[nr + (nc - 1L) * rows]]
        ndi <- (s + 1L) + max_straight * ((d - 1L) + 4L * ((nc - 1L) + cols * (nr - 1L)))
        if (ncost < dist[[ndi]]) {
          dist[[ndi]] <- ncost
          ncount <- ncount + 1L
          nodes[[ncount]] <- ndi
          nxt[[ncount]] <- 0L
          nb <- ncost %% 10L + 1L
          if (head[[nb]] == 0L) head[[nb]] <- ncount else nxt[[tail[[nb]]]] <- ncount
          tail[[nb]] <- ncount
          if (ncost > max_cost) max_cost <- ncost
        }
      }
    }

    if (s >= min_straight) {
      nd <- left[[d]]
      nr <- r + dr[[nd]]
      nc <- c + dc[[nd]]
      if (nr >= 1L && nr <= rows && nc >= 1L && nc <= cols) {
        ncost <- cost + grid[[nr + (nc - 1L) * rows]]
        ndi <- 1L + max_straight * ((nd - 1L) + 4L * ((nc - 1L) + cols * (nr - 1L)))
        if (ncost < dist[[ndi]]) {
          dist[[ndi]] <- ncost
          ncount <- ncount + 1L
          nodes[[ncount]] <- ndi
          nxt[[ncount]] <- 0L
          nb <- ncost %% 10L + 1L
          if (head[[nb]] == 0L) head[[nb]] <- ncount else nxt[[tail[[nb]]]] <- ncount
          tail[[nb]] <- ncount
          if (ncost > max_cost) max_cost <- ncost
        }
      }
      nd <- right[[d]]
      nr <- r + dr[[nd]]
      nc <- c + dc[[nd]]
      if (nr >= 1L && nr <= rows && nc >= 1L && nc <= cols) {
        ncost <- cost + grid[[nr + (nc - 1L) * rows]]
        ndi <- 1L + max_straight * ((nd - 1L) + 4L * ((nc - 1L) + cols * (nr - 1L)))
        if (ncost < dist[[ndi]]) {
          dist[[ndi]] <- ncost
          ncount <- ncount + 1L
          nodes[[ncount]] <- ndi
          nxt[[ncount]] <- 0L
          nb <- ncost %% 10L + 1L
          if (head[[nb]] == 0L) head[[nb]] <- ncount else nxt[[tail[[nb]]]] <- ncount
          tail[[nb]] <- ncount
          if (ncost > max_cost) max_cost <- ncost
        }
      }
    }
  }

  stop("No path to the factory")
}
least_heat <- compiler::cmpfun(least_heat)

part_one_solver <- function(lines) {
  least_heat(parse_grid(lines), min_straight = 1L, max_straight = 3L)
}

part_two_solver <- function(lines) {
  least_heat(parse_grid(lines), min_straight = 4L, max_straight = 10L)
}

example <- c(
  "2413432311323",
  "3215453535623",
  "3255245654254",
  "3446585845452",
  "4546657867536",
  "1438598798454",
  "4457876987766",
  "3637877979653",
  "4654967986887",
  "4564679986453",
  "1224686865563",
  "2546548887735",
  "4322674655533"
)
example_loss <- c(
  "111111111111",
  "999999999991",
  "999999999991",
  "999999999991",
  "999999999991"
)
stopifnot(part_one_solver(example) == 102L)
stopifnot(part_two_solver(example) == 94L)
stopifnot(part_two_solver(example_loss) == 71L)

res_01 <- part_one_solver(input)
res_02 <- part_two_solver(input)

cat(sprintf("Part 1 answer: %s\n", format(res_01, scientific = FALSE)))
cat(sprintf("Part 2 answer: %s\n", format(res_02, scientific = FALSE)))
