source("src/utils.R")

# --- Day 22: Sand Slabs ---

input <- read_input("day_22")

# Each brick is an axis-aligned segment. Ends are ordered so the first
# coordinate of each axis is the smaller one.
parse_bricks <- function(lines) {
  bricks <- t(vapply(lines, function(line) {
    nums <- as.integer(strsplit(gsub("~", ",", line, fixed = TRUE), ",", fixed = TRUE)[[1L]])
    if (nums[[1L]] > nums[[4L]]) nums[c(1L, 4L)] <- nums[c(4L, 1L)]
    if (nums[[2L]] > nums[[5L]]) nums[c(2L, 5L)] <- nums[c(5L, 2L)]
    if (nums[[3L]] > nums[[6L]]) nums[c(3L, 6L)] <- nums[c(6L, 3L)]
    nums
  }, integer(6L)))
  bricks[order(bricks[, 3L]), , drop = FALSE]
}

# Drop bricks in bottom-up order. A brick rests on the tallest cube under its
# footprint and is supported by every brick that provides that height.
settle <- function(bricks) {
  n <- nrow(bricks)
  height <- matrix(0L, max(bricks[, c(1L, 4L)]) + 1L, max(bricks[, c(2L, 5L)]) + 1L)
  top_id <- matrix(0L, nrow(height), ncol(height))
  supporters <- vector("list", n)

  for (i in seq_len(n)) {
    x1 <- bricks[[i, 1L]]
    y1 <- bricks[[i, 2L]]
    z1 <- bricks[[i, 3L]]
    x2 <- bricks[[i, 4L]]
    y2 <- bricks[[i, 5L]]
    z2 <- bricks[[i, 6L]]
    if (x1 != x2) {
      cells <- cbind(x1:x2, y1)
    } else if (y1 != y2) {
      cells <- cbind(x1, y1:y2)
    } else {
      cells <- cbind(x1, y1)
    }
    idx <- cbind(cells[, 1L] + 1L, cells[, 2L] + 1L)
    under <- height[idx]
    peak <- max(under)
    ids <- unique(top_id[idx][under == peak])
    supporters[[i]] <- ids[ids != 0L]
    top <- peak + 1L + (z2 - z1)
    height[idx] <- top
    top_id[idx] <- i
  }

  supporters
}

# A brick is safe when every brick it supports still has another support.
count_safe <- function(supporters) {
  sole <- unique(unlist(supporters[lengths(supporters) == 1L], use.names = FALSE))
  length(supporters) - length(sole)
}

# Bricks are indexed from the ground up, so a brick falls only after every
# brick supporting it has already fallen. Sum, over each disintegrated brick,
# how many other bricks that would bring down.
chain_reaction <- function(supporters) {
  n <- length(supporters)
  above <- vector("list", n)
  for (i in seq_len(n)) {
    for (below in supporters[[i]]) above[[below]] <- c(above[[below]], i)
  }

  total <- 0L
  fallen <- rep(FALSE, n)
  queue <- integer(n)
  for (start in seq_len(n)) {
    if (length(above[[start]]) == 0L) next
    fallen[] <- FALSE
    fallen[[start]] <- TRUE
    head <- 1L
    tail <- 1L
    queue[[1L]] <- start
    while (head <= tail) {
      below <- queue[[head]]
      head <- head + 1L
      for (brick in above[[below]]) {
        if (!fallen[[brick]] && all(fallen[supporters[[brick]]])) {
          fallen[[brick]] <- TRUE
          total <- total + 1L
          tail <- tail + 1L
          queue[[tail]] <- brick
        }
      }
    }
  }
  total
}

example <- c(
  "1,0,1~1,2,1",
  "0,0,2~2,0,2",
  "0,2,3~2,2,3",
  "0,0,4~0,2,4",
  "2,0,5~2,2,5",
  "0,1,6~2,1,6",
  "1,1,8~1,1,9"
)
example_supporters <- settle(parse_bricks(example))
stopifnot(count_safe(example_supporters) == 5)
stopifnot(chain_reaction(example_supporters) == 7)

supporters <- settle(parse_bricks(input))
res_01 <- count_safe(supporters)
res_02 <- chain_reaction(supporters)

cat(sprintf("Part 1 answer: %s\n", format(res_01, scientific = FALSE)))
cat(sprintf("Part 2 answer: %s\n", format(res_02, scientific = FALSE)))
