source("src/utils.R")

# --- Day 05: If You Give A Seed A Fertilizer ---

input <- read_input("day_05")

# Seeds, then each category map as a matrix of destination, source, length
parse_almanac <- function(lines) {
  lines <- lines[nzchar(lines)]
  seeds <- as.numeric(strsplit(sub("^seeds:\\s*", "", lines[[1]]), "\\s+")[[1]])
  maps <- list()
  ranges <- list()

  for (line in lines[-1]) {
    if (grepl("map:\\s*$", line)) {
      if (length(ranges) > 0) {
        maps[[length(maps) + 1]] <- ranges
      }
      ranges <- list()
    } else {
      ranges[[length(ranges) + 1]] <- as.numeric(strsplit(line, "\\s+")[[1]])
    }
  }
  if (length(ranges) > 0) {
    maps[[length(maps) + 1]] <- ranges
  }

  maps <- lapply(maps, function(ranges) {
    mat <- do.call(rbind, ranges)
    mat[order(mat[, 2]), , drop = FALSE]
  })

  list(seeds = seeds, maps = maps)
}

# One source number through one map. Unmapped numbers stay the same.
apply_map <- function(n, ranges) {
  for (i in seq_len(nrow(ranges))) {
    src <- ranges[i, 2]
    len <- ranges[i, 3]
    if (n >= src && n < src + len) {
      return(ranges[i, 1] + (n - src))
    }
  }
  n
}

part_one_solver <- function(lines) {
  almanac <- parse_almanac(lines)
  locations <- vapply(almanac$seeds, function(seed) {
    value <- seed
    for (ranges in almanac$maps) {
      value <- apply_map(value, ranges)
    }
    value
  }, numeric(1))
  min(locations)
}

# Shift every half-open interval [start, end) through one map
map_intervals <- function(intervals, ranges) {
  mapped <- list()
  for (i in seq_len(nrow(intervals))) {
    cursor <- intervals$start[[i]]
    end <- intervals$end[[i]]
    for (j in seq_len(nrow(ranges))) {
      src <- ranges[j, 2]
      src_end <- src + ranges[j, 3]
      if (src_end <= cursor) next
      if (src >= end) break
      if (src > cursor) {
        gap_end <- min(src, end)
        mapped[[length(mapped) + 1]] <- c(cursor, gap_end)
        cursor <- gap_end
      }
      overlap_end <- min(end, src_end)
      if (cursor < overlap_end) {
        offset <- ranges[j, 1] - src
        mapped[[length(mapped) + 1]] <- c(cursor + offset, overlap_end + offset)
        cursor <- overlap_end
      }
      if (cursor >= end) break
    }
    if (cursor < end) {
      mapped[[length(mapped) + 1]] <- c(cursor, end)
    }
  }

  mat <- do.call(rbind, mapped)
  data.frame(start = mat[, 1], end = mat[, 2])
}

# Part 2 reads seeds as start/length pairs and keeps them as ranges
part_two_solver <- function(lines) {
  almanac <- parse_almanac(lines)
  seeds <- almanac$seeds
  starts <- seeds[seq(1, length(seeds), by = 2)]
  lengths <- seeds[seq(2, length(seeds), by = 2)]
  intervals <- data.frame(start = starts, end = starts + lengths)

  for (ranges in almanac$maps) {
    intervals <- map_intervals(intervals, ranges)
  }

  min(intervals$start)
}

res_01 <- part_one_solver(input)
res_02 <- part_two_solver(input)

# Output results
cat(sprintf("Part 1 answer: %s\n", res_01))
cat(sprintf("Part 2 answer: %s\n", res_02))
