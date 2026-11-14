source("src/utils.R")

# --- Day 03: Gear Ratios ---

input <- read_input("day_03")

# Every number in the schematic, with the columns it occupies
find_numbers <- function(lines) {
  do.call(rbind, lapply(seq_along(lines), function(row) {
    matches <- gregexpr("\\d+", lines[[row]])[[1]]
    if (matches[[1]] == -1) {
      return(NULL)
    }
    lengths <- attr(matches, "match.length")
    data.frame(
      row = row,
      start = as.integer(matches),
      end = as.integer(matches + lengths - 1L),
      value = as.numeric(substring(lines[[row]], matches, matches + lengths - 1L))
    )
  }))
}

# Cells around a number, including the number itself, clamped to the grid
neighborhood <- function(num, nrows, ncols) {
  r0 <- max(1L, num$row - 1L)
  r1 <- min(nrows, num$row + 1L)
  c0 <- max(1L, num$start - 1L)
  c1 <- min(ncols, num$end + 1L)
  list(rows = r0:r1, cols = c0:c1)
}

part_one_solver <- function(lines) {
  grid <- strsplit(lines, "", fixed = TRUE)
  nrows <- length(grid)
  ncols <- length(grid[[1]])
  numbers <- find_numbers(lines)

  part_sum <- 0
  for (i in seq_len(nrow(numbers))) {
    num <- numbers[i, ]
    area <- neighborhood(num, nrows, ncols)
    touches_symbol <- FALSE
    for (r in area$rows) {
      for (c in area$cols) {
        ch <- grid[[r]][[c]]
        if (!grepl("[0-9.]", ch)) {
          touches_symbol <- TRUE
          break
        }
      }
      if (touches_symbol) break
    }
    if (touches_symbol) {
      part_sum <- part_sum + num$value
    }
  }

  part_sum
}

# A gear is a "*" touching exactly two part numbers; its ratio is their product
part_two_solver <- function(lines) {
  grid <- strsplit(lines, "", fixed = TRUE)
  nrows <- length(grid)
  ncols <- length(grid[[1]])
  numbers <- find_numbers(lines)
  gear_parts <- list()

  for (i in seq_len(nrow(numbers))) {
    num <- numbers[i, ]
    area <- neighborhood(num, nrows, ncols)
    for (r in area$rows) {
      for (c in area$cols) {
        if (grid[[r]][[c]] == "*") {
          key <- paste(r, c, sep = ",")
          gear_parts[[key]] <- c(gear_parts[[key]], num$value)
        }
      }
    }
  }

  ratios <- vapply(gear_parts, function(parts) {
    if (length(parts) == 2L) parts[[1]] * parts[[2]] else 0
  }, numeric(1))

  sum(ratios)
}

res_01 <- part_one_solver(input)
res_02 <- part_two_solver(input)

# Output results
cat(sprintf("Part 1 answer: %s\n", res_01))
cat(sprintf("Part 2 answer: %s\n", res_02))
