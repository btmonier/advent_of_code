source("src/utils.R")

# --- Day 21: Step Counter ---

input <- read_input("day_21")

# Each step moves north, south, east, or west onto a garden plot. `steps` may
# be several lengths; each result is the number of plots occupied after exactly
# that many steps. Cells outside `rocks` are walls.
plots_at_steps <- function(rocks, start_row, start_col, steps) {
  rows <- nrow(rocks)
  cols <- ncol(rocks)
  cur <- matrix(FALSE, rows, cols)
  nxt <- matrix(FALSE, rows, cols)
  cur[start_row, start_col] <- TRUE

  wanted <- sort(unique(as.integer(steps)))
  counts <- setNames(rep(NA_real_, length(wanted)), wanted)
  if (any(wanted == 0L)) counts[["0"]] <- 1

  for (step in seq_len(max(wanted))) {
    nxt[] <- FALSE
    nxt[-rows, ] <- cur[-1L, ]
    nxt[-1L, ] <- nxt[-1L, ] | cur[-rows, ]
    nxt[, -cols] <- nxt[, -cols] | cur[, -1L]
    nxt[, -1L] <- nxt[, -1L] | cur[, -cols]
    nxt[rocks] <- FALSE
    tmp <- cur
    cur <- nxt
    nxt <- tmp
    if (step %in% wanted) counts[[as.character(step)]] <- sum(cur)
  }

  as.numeric(counts[as.character(steps)])
}

reachable_plots <- function(lines, steps) {
  grid <- do.call(rbind, strsplit(lines, "", fixed = TRUE))
  start <- which(grid == "S", arr.ind = TRUE)
  plots_at_steps(grid == "#", start[[1L]], start[[2L]], steps)
}

# The same walk on a map tiled in every direction. Repeated S tiles are plots.
infinite_plots <- function(lines, steps) {
  grid <- do.call(rbind, strsplit(lines, "", fixed = TRUE))
  rows <- nrow(grid)
  cols <- ncol(grid)
  start <- which(grid == "S", arr.ind = TRUE)
  reach <- max(steps)
  span <- reach * 2L + 1L
  offset <- reach + 1L
  ri <- ((seq_len(span) - offset + start[[1L]] - 1L) %% rows) + 1L
  ci <- ((seq_len(span) - offset + start[[2L]] - 1L) %% cols) + 1L
  plots_at_steps((grid == "#")[ri, ci], offset, offset, steps)
}

# The real map is an odd square with an empty center row, center column, and
# border, so the reachable count at half + n * size steps is a quadratic in n.
plots_after_many_steps <- function(lines, total_steps) {
  size <- nchar(lines[[1L]])
  half <- size %/% 2L
  stopifnot(length(lines) == size, (total_steps - half) %% size == 0)
  n <- (total_steps - half) / size

  counts <- infinite_plots(lines, half + (0:3) * size)
  y0 <- counts[[1L]]
  y1 <- counts[[2L]]
  y2 <- counts[[3L]]
  y3 <- counts[[4L]]
  a <- (y2 - 2 * y1 + y0) / 2
  b <- y1 - y0 - a
  stopifnot(a == floor(a), a * 9 + b * 3 + y0 == y3)
  a * n * n + b * n + y0
}

example <- c(
  "...........",
  ".....###.#.",
  ".###.##..#.",
  "..#.#...#..",
  "....#.#....",
  ".##..S####.",
  ".##..#...#.",
  ".......##..",
  ".##.#.####.",
  ".##..##.##.",
  "..........."
)
stopifnot(reachable_plots(example, 6L) == 16)
stopifnot(infinite_plots(example, c(6L, 10L, 50L, 100L)) == c(16, 50, 1594, 6536))

res_01 <- reachable_plots(input, 64L)
res_02 <- plots_after_many_steps(input, 26501365)

cat(sprintf("Part 1 answer: %s\n", format(res_01, scientific = FALSE)))
cat(sprintf("Part 2 answer: %s\n", format(res_02, scientific = FALSE, trim = TRUE)))
