source("src/utils.R")

# --- Day 23: A Long Walk ---

input <- read_input("day_23")

# Open neighbors of a trail tile. Slopes are one-way when slippery is TRUE.
outgoing <- function(grid, r, c, slippery) {
  rows <- nrow(grid)
  cols <- ncol(grid)
  tile <- grid[[r, c]]
  if (slippery && tile == ">") {
    steps <- cbind(r, c + 1L)
  } else if (slippery && tile == "<") {
    steps <- cbind(r, c - 1L)
  } else if (slippery && tile == "v") {
    steps <- cbind(r + 1L, c)
  } else if (slippery && tile == "^") {
    steps <- cbind(r - 1L, c)
  } else {
    steps <- cbind(c(r - 1L, r + 1L, r, r), c(c, c, c - 1L, c + 1L))
  }
  inside <- steps[, 1L] >= 1L & steps[, 1L] <= rows &
    steps[, 2L] >= 1L & steps[, 2L] <= cols
  steps <- steps[inside, , drop = FALSE]
  if (nrow(steps) == 0L) return(steps)
  steps[grid[steps] != "#", , drop = FALSE]
}

# Corridors between junctions are contracted to weighted edges. The longest hike
# is the longest path through that graph that never repeats a junction.
longest_hike <- function(lines, slippery = TRUE) {
  grid <- do.call(rbind, strsplit(lines, "", fixed = TRUE))
  rows <- nrow(grid)
  cols <- ncol(grid)
  start_c <- which(grid[1L, ] == ".")
  end_c <- which(grid[rows, ] == ".")
  junction_id <- matrix(0L, rows, cols)
  id <- 0L

  for (r in seq_len(rows)) {
    for (c in seq_len(cols)) {
      if (grid[[r, c]] == "#") next
      branch <- nrow(outgoing(grid, r, c, slippery = FALSE)) != 2L
      if (branch || (r == 1L && c == start_c) || (r == rows && c == end_c)) {
        id <- id + 1L
        junction_id[[r, c]] <- id
      }
    }
  }

  dist_mat <- matrix(0L, id, id)
  for (r in seq_len(rows)) {
    for (c in seq_len(cols)) {
      from <- junction_id[[r, c]]
      if (from == 0L) next
      outs <- outgoing(grid, r, c, slippery)
      if (nrow(outs) == 0L) next
      for (k in seq_len(nrow(outs))) {
        prev_r <- r
        prev_c <- c
        cur_r <- outs[[k, 1L]]
        cur_c <- outs[[k, 2L]]
        steps <- 1L
        valid <- TRUE
        while (junction_id[[cur_r, cur_c]] == 0L) {
          nbrs <- outgoing(grid, cur_r, cur_c, slippery)
          keep <- !(nbrs[, 1L] == prev_r & nbrs[, 2L] == prev_c)
          nbrs <- nbrs[keep, , drop = FALSE]
          if (nrow(nbrs) != 1L) {
            valid <- FALSE
            break
          }
          prev_r <- cur_r
          prev_c <- cur_c
          cur_r <- nbrs[[1L, 1L]]
          cur_c <- nbrs[[1L, 2L]]
          steps <- steps + 1L
        }
        if (!valid) next
        dest <- junction_id[[cur_r, cur_c]]
        if (steps > dist_mat[[from, dest]]) dist_mat[[from, dest]] <- steps
      }
    }
  }

  tos <- vector("list", id)
  wts <- vector("list", id)
  for (node in seq_len(id)) {
    nxt <- which(dist_mat[node, ] > 0L)
    tos[[node]] <- nxt
    wts[[node]] <- dist_mat[node, nxt]
  }

  goal <- junction_id[[rows, end_c]]
  start <- junction_id[[1L, start_c]]
  visited <- rep(FALSE, id)
  best <- 0L

  # On the dry map every corridor can be walked either way, so the unused
  # corridor lengths are an upper bound on how much farther the hike can go.
  if (!slippery) {
    remain <- sum(dist_mat) / 2
    search <- function(node, dist, remain) {
      if (node == goal) {
        if (dist > best) best <<- dist
        return()
      }
      if (dist + remain <= best) return()
      visited[[node]] <<- TRUE
      nbrs <- tos[[node]]
      weights <- wts[[node]]
      for (k in seq_along(nbrs)) {
        nxt <- nbrs[[k]]
        if (visited[[nxt]]) next
        lost <- weights[[k]]
        ahead <- tos[[nxt]]
        ahead_w <- wts[[nxt]]
        for (t in seq_along(ahead)) {
          if (ahead[[t]] != node && visited[[ahead[[t]]]]) lost <- lost + ahead_w[[t]]
        }
        search(nxt, dist + weights[[k]], remain - lost)
      }
      visited[[node]] <<- FALSE
    }
    search(start, 0, remain)
  } else {
    search <- function(node, dist) {
      if (node == goal) {
        if (dist > best) best <<- dist
        return()
      }
      visited[[node]] <<- TRUE
      nbrs <- tos[[node]]
      weights <- wts[[node]]
      for (k in seq_along(nbrs)) {
        nxt <- nbrs[[k]]
        if (!visited[[nxt]]) search(nxt, dist + weights[[k]])
      }
      visited[[node]] <<- FALSE
    }
    search(start, 0L)
  }
  best
}

example <- c(
  "#.#####################",
  "#.......#########...###",
  "#######.#########.#.###",
  "###.....#.>.>.###.#.###",
  "###v#####.#v#.###.#.###",
  "###.>...#.#.#.....#...#",
  "###v###.#.#.#########.#",
  "###...#.#.#.......#...#",
  "#####.#.#.#######.#.###",
  "#.....#.#.#.......#...#",
  "#.#####.#.#.#########v#",
  "#.#...#...#...###...>.#",
  "#.#.#v#######v###.###v#",
  "#...#.>.#...>.>.#.###.#",
  "#####v#.#.###v#.#.###.#",
  "#.....#...#...#.#.#...#",
  "#.#########.###.#.#.###",
  "#...###...#...#...#.###",
  "###.###.#.###v#####v###",
  "#...#...#.#.>.>.#.>.###",
  "#.###.###.#.###.#.#v###",
  "#.....###...###...#...#",
  "#####################.#"
)
stopifnot(longest_hike(example) == 94)
stopifnot(longest_hike(example, slippery = FALSE) == 154)

res_01 <- longest_hike(input)
res_02 <- longest_hike(input, slippery = FALSE)

cat(sprintf("Part 1 answer: %s\n", format(res_01, scientific = FALSE)))
cat(sprintf("Part 2 answer: %s\n", format(res_02, scientific = FALSE)))
