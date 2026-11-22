source("src/utils.R")

# --- Day 10: Pipe Maze ---

input <- read_input("day_10")

# Openings of each pipe. Rows increase south, columns increase east.
pipe_exits <- list(
  "|" = c("N", "S"),
  "-" = c("E", "W"),
  "L" = c("N", "E"),
  "J" = c("N", "W"),
  "7" = c("S", "W"),
  "F" = c("S", "E")
)

delta <- list(
  N = c(-1L, 0L),
  S = c(1L, 0L),
  E = c(0L, 1L),
  W = c(0L, -1L)
)
opposite <- c(N = "S", S = "N", E = "W", W = "E")

parse_grid <- function(lines) {
  do.call(rbind, strsplit(lines, ""))
}

find_start <- function(grid) {
  pos <- which(grid == "S", arr.ind = TRUE)
  c(pos[1, 1], pos[1, 2])
}

in_bounds <- function(grid, row, col) {
  row >= 1L && row <= nrow(grid) && col >= 1L && col <= ncol(grid)
}

step <- function(row, col, dir) {
  c(row + delta[[dir]][[1]], col + delta[[dir]][[2]])
}

# Tiles whose pipe opens back toward (row, col). S has exactly two of these.
connections <- function(grid, row, col) {
  linked <- list()
  for (dir in names(delta)) {
    nxt <- step(row, col, dir)
    if (!in_bounds(grid, nxt[[1]], nxt[[2]])) next
    exits <- pipe_exits[[grid[nxt[[1]], nxt[[2]]]]]
    if (!is.null(exits) && opposite[[dir]] %in% exits) {
      linked[[length(linked) + 1]] <- nxt
    }
  }
  linked
}

# The other end of the pipe at `pos`. Loop tiles have two openings, and one is `prev`.
next_pos <- function(grid, pos, prev) {
  for (dir in pipe_exits[[grid[pos[[1]], pos[[2]]]]]) {
    nxt <- step(pos[[1]], pos[[2]], dir)
    if (nxt[[1]] != prev[[1]] || nxt[[2]] != prev[[2]]) return(nxt)
  }
}

direction_to <- function(from, to) {
  for (dir in names(delta)) {
    nxt <- step(from[[1]], from[[2]], dir)
    if (nxt[[1]] == to[[1]] && nxt[[2]] == to[[2]]) return(dir)
  }
}

# The two pipes that meet S are its openings, which identify the hidden shape.
pipe_from_openings <- function(dirs) {
  openings <- sort(dirs)
  if (identical(openings, c("N", "S"))) return("|")
  if (identical(openings, c("E", "W"))) return("-")
  if (identical(openings, c("E", "N"))) return("L")
  if (identical(openings, c("N", "W"))) return("J")
  if (identical(openings, c("S", "W"))) return("7")
  if (identical(openings, c("E", "S"))) return("F")
  stop("S is not connected to two pipes")
}

# Grid with S drawn as its real pipe, and the tiles that belong to the loop.
trace_loop <- function(lines) {
  grid <- parse_grid(lines)
  start <- find_start(grid)
  neighbors <- connections(grid, start[[1]], start[[2]])
  stopifnot(length(neighbors) == 2L)

  dirs <- vapply(neighbors, function(pos) direction_to(start, pos), character(1))
  grid[start[[1]], start[[2]]] <- pipe_from_openings(dirs)

  on_loop <- matrix(FALSE, nrow(grid), ncol(grid))
  on_loop[start[[1]], start[[2]]] <- TRUE
  prev <- start
  pos <- neighbors[[1]]
  repeat {
    on_loop[pos[[1]], pos[[2]]] <- TRUE
    nxt <- next_pos(grid, pos, prev)
    if (nxt[[1]] == start[[1]] && nxt[[2]] == start[[2]]) break
    prev <- pos
    pos <- nxt
  }
  list(grid = grid, on_loop = on_loop)
}

# The loop is one cycle, so the farther way around is the long way and the
# farthest tile is halfway along the loop.
part_one_solver <- function(lines) {
  sum(trace_loop(lines)$on_loop) %/% 2L
}

# A left-to-right ray crosses the loop on a pipe that opens north.
# L---J and F---7 bend back to the same side; L---7 and F---J cross once.
# Pipes that are not on the loop never count, so junk tiles can still be inside.
part_two_solver <- function(lines) {
  traced <- trace_loop(lines)
  grid <- traced$grid
  on_loop <- traced$on_loop
  enclosed <- 0L

  for (row in seq_len(nrow(grid))) {
    inside <- FALSE
    for (col in seq_len(ncol(grid))) {
      if (on_loop[row, col]) {
        if (grid[row, col] %in% c("|", "L", "J")) inside <- !inside
      } else if (inside) {
        enclosed <- enclosed + 1L
      }
    }
  }
  enclosed
}

example_square <- c(
  ".....",
  ".S-7.",
  ".|.|.",
  ".L-J.",
  "....."
)
example_square_junk <- c(
  "-L|F7",
  "7S-7|",
  "L|7||",
  "-L-J|",
  "L|-JF"
)
example_complex <- c(
  "..F7.",
  ".FJ|.",
  "SJ.L7",
  "|F--J",
  "LJ..."
)
example_complex_junk <- c(
  "7-F7-",
  ".FJ|7",
  "SJLL7",
  "|F--J",
  "LJ.LJ"
)
example_nest <- c(
  "...........",
  ".S-------7.",
  ".|F-----7|.",
  ".||.....||.",
  ".||.....||.",
  ".|L-7.F-J|.",
  ".|..|.|..|.",
  ".L--J.L--J.",
  "..........."
)
example_squeeze <- c(
  "..........",
  ".S------7.",
  ".|F----7|.",
  ".||....||.",
  ".||....||.",
  ".|L-7F-J|.",
  ".|..||..|.",
  ".L--JL--J.",
  ".........."
)
example_large <- c(
  ".F----7F7F7F7F-7....",
  ".|F--7||||||||FJ....",
  ".||.FJ||||||||L7....",
  "FJL7L7LJLJ||LJ.L-7..",
  "L--J.L7...LJS7F-7L7.",
  "....F-J..F7FJ|L7L7L7",
  "....L7.F7||L7|.L7L7|",
  ".....|FJLJ|FJ|F7|.LJ",
  "....FJL-7.||.||||...",
  "....L---J.LJ.LJLJ..."
)
example_junk <- c(
  "FF7FSF7F7F7F7F7F---7",
  "L|LJ||||||||||||F--J",
  "FL-7LJLJ||||||LJL-77",
  "F--JF--7||LJLJ7F7FJ-",
  "L---JF-JLJ.||-FJLJJ7",
  "|F|F-JF---7F7-L7L|7|",
  "|FFJF7L7F-JF7|JL---7",
  "7-L-JL7||F7|L7F-7F7|",
  "L.L7LFJ|||||FJL7||LJ",
  "L7JLJL-JLJLJL--JLJ.L"
)
stopifnot(part_one_solver(example_square) == 4)
stopifnot(part_one_solver(example_square_junk) == 4)
stopifnot(part_one_solver(example_complex) == 8)
stopifnot(part_one_solver(example_complex_junk) == 8)
stopifnot(part_two_solver(example_nest) == 4)
stopifnot(part_two_solver(example_squeeze) == 4)
stopifnot(part_two_solver(example_large) == 8)
stopifnot(part_two_solver(example_junk) == 10)

res_01 <- part_one_solver(input)
res_02 <- part_two_solver(input)

cat(sprintf("Part 1 answer: %s\n", format(res_01, scientific = FALSE)))
cat(sprintf("Part 2 answer: %s\n", format(res_02, scientific = FALSE)))
