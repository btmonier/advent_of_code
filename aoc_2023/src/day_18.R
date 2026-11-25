source("src/utils.R")

# --- Day 18: Lavaduct Lagoon ---

input <- read_input("day_18")

# Rows increase downward. Columns increase to the right.
steps <- list(
  U = c(-1L, 0L),
  D = c(1L, 0L),
  L = c(0L, -1L),
  R = c(0L, 1L)
)

parse_plan <- function(lines) {
  parts <- strsplit(lines, " ", fixed = TRUE)
  list(
    dir = vapply(parts, `[[`, character(1), 1L),
    dist = vapply(parts, function(part) as.integer(part[[2L]]), integer(1))
  )
}

# 0 means R, 1 means D, 2 means L, and 3 means U. The first five hex digits
# are the distance.
hex_dirs <- c("0" = "R", "1" = "D", "2" = "L", "3" = "U")

parse_hex_plan <- function(lines) {
  hex <- sub(".*#([0-9a-fA-F]{6}).*", "\\1", lines)
  list(
    dir = unname(hex_dirs[substr(hex, 6L, 6L)]),
    dist = strtoi(substr(hex, 1L, 5L), 16L)
  )
}

# Shoelace sums can pass 2^53, so the running total is stored as
# hi * 1e9 + lo with lo in [0, 1e9). Each corner product still fits in a double.
BASE <- 1e9

add_term <- function(hi, lo, term) {
  hi <- hi + term %/% BASE
  lo <- lo + term %% BASE
  if (lo >= BASE) {
    lo <- lo - BASE
    hi <- hi + 1
  }
  c(hi, lo)
}

abs_pair <- function(hi, lo) {
  if (hi > 0 || hi == 0) return(c(hi, lo))
  if (lo == 0) return(c(-hi, 0))
  c(-hi - 1, BASE - lo)
}

div2 <- function(hi, lo) {
  new_hi <- hi %/% 2
  new_lo <- (hi %% 2) * (BASE / 2) + lo / 2
  if (new_lo >= BASE) {
    new_lo <- new_lo - BASE
    new_hi <- new_hi + 1
  }
  c(new_hi, new_lo)
}

format_pair <- function(hi, lo) {
  if (hi == 0) return(sprintf("%.0f", lo))
  sprintf("%.0f%09.0f", hi, lo)
}

# The trench is the boundary of a polygon on the grid. Shoelace measures the
# interior, and Pick's theorem adds the dug boundary cubes:
# volume = area + perimeter / 2 + 1.
lagoon_volume <- function(dir, dist) {
  dist <- as.numeric(dist)
  x <- 0
  y <- 0
  hi <- 0
  lo <- 0
  for (i in seq_along(dist)) {
    step <- steps[[dir[[i]]]]
    dx <- step[[2L]] * dist[[i]]
    dy <- step[[1L]] * dist[[i]]
    total <- add_term(hi, lo, x * dy - y * dx)
    hi <- total[[1L]]
    lo <- total[[2L]]
    x <- x + dx
    y <- y + dy
  }
  stopifnot(x == 0, y == 0)

  total <- abs_pair(hi, lo)
  total <- add_term(total[[1L]], total[[2L]], sum(dist))
  total <- div2(total[[1L]], total[[2L]])
  total <- add_term(total[[1L]], total[[2L]], 1)
  format_pair(total[[1L]], total[[2L]])
}

part_one_solver <- function(lines) {
  plan <- parse_plan(lines)
  lagoon_volume(plan$dir, plan$dist)
}

part_two_solver <- function(lines) {
  plan <- parse_hex_plan(lines)
  lagoon_volume(plan$dir, plan$dist)
}

plan_colors <- function(lines) {
  paste0("#", tolower(sub(".*#([0-9a-fA-F]{6}).*", "\\1", lines)))
}

# Each dig instruction becomes one straight edge. The hex code is the RGB color
# the Elves paint on that edge; in part 2 the same code also chooses its length
# and direction.
trench_segments <- function(dir, dist, color, plan) {
  dist <- as.numeric(dist)
  n <- length(dist)
  x <- numeric(n)
  y <- numeric(n)
  xend <- numeric(n)
  yend <- numeric(n)
  cursor_x <- 0
  cursor_y <- 0
  for (i in seq_len(n)) {
    step <- steps[[dir[[i]]]]
    x[[i]] <- cursor_x
    y[[i]] <- cursor_y
    cursor_x <- cursor_x + step[[2L]] * dist[[i]]
    cursor_y <- cursor_y + step[[1L]] * dist[[i]]
    xend[[i]] <- cursor_x
    yend[[i]] <- cursor_y
  }
  data.frame(
    x = x, y = y, xend = xend, yend = yend,
    color = color, plan = plan,
    stringsAsFactors = FALSE
  )
}

lagoon_panel <- function(segments, title) {
  vertices <- data.frame(
    x = c(segments$x, segments$xend[[nrow(segments)]]),
    y = c(segments$y, segments$yend[[nrow(segments)]])
  )
  ggplot2::ggplot() +
    ggplot2::geom_polygon(
      data = vertices,
      ggplot2::aes(x = x, y = y),
      fill = "#16324a",
      colour = NA
    ) +
    ggplot2::geom_segment(
      data = segments,
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend, colour = color),
      linewidth = 1,
      lineend = "round"
    ) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_y_reverse() +
    ggplot2::coord_fixed() +
    ggplot2::labs(title = title) +
    ggplot2::theme_void(base_size = 13) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "#0f1419", colour = NA),
      panel.background = ggplot2::element_rect(fill = "#0f1419", colour = NA),
      plot.title = ggplot2::element_text(
        colour = "#e8eef2", hjust = 0.5,
        margin = ggplot2::margin(8, 8, 10, 8)
      ),
      plot.margin = ggplot2::margin(4, 10, 10, 10)
    )
}

# Part 1 and part 2 cover very different distances, so each panel keeps its own scale.
plot_lagoons <- function(lines) {
  colors <- plan_colors(lines)
  literal <- parse_plan(lines)
  decoded <- parse_hex_plan(lines)
  list(
    lagoon_panel(
      trench_segments(literal$dir, literal$dist, colors, "part 1"),
      "Part 1: edges painted with the color codes"
    ),
    lagoon_panel(
      trench_segments(decoded$dir, decoded$dist, colors, "part 2"),
      "Part 2: shape decoded from those same codes"
    )
  )
}

save_lagoon_plot <- function(lines, path = "plots/day_18_lagoon.png") {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  panels <- plot_lagoons(lines)
  grDevices::png(path, width = 12, height = 6.6, units = "in", res = 160, bg = "#0f1419")
  on.exit(grDevices::dev.off(), add = TRUE)
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(
    nrow = 2, ncol = 2,
    heights = grid::unit(c(0.16, 0.84), "npc")
  )))
  grid::pushViewport(grid::viewport(layout.pos.row = 1, layout.pos.col = 1:2))
  grid::grid.text(
    "Lavaduct lagoon",
    y = 0.72,
    gp = grid::gpar(col = "#f4f7f8", fontsize = 18, fontface = "bold")
  )
  grid::grid.text(
    "Trench edges are drawn in the dig plan's hex colors",
    y = 0.28,
    gp = grid::gpar(col = "#b7c3cc", fontsize = 12)
  )
  grid::popViewport()
  print(panels[[1]], vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 1))
  print(panels[[2]], vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 2))
  invisible(path)
}

example <- c(
  "R 6 (#70c710)",
  "D 5 (#0dc571)",
  "L 2 (#5713f0)",
  "D 2 (#d2c081)",
  "R 2 (#59c680)",
  "D 2 (#411b91)",
  "L 5 (#8ceee2)",
  "U 2 (#caa173)",
  "L 1 (#1b58a2)",
  "U 2 (#caa171)",
  "R 2 (#7807d2)",
  "U 3 (#a77fa3)",
  "L 2 (#015232)",
  "U 2 (#7a21e3)"
)
example_hex <- c(
  "R 461937",
  "D 56407",
  "R 356671",
  "D 863240",
  "R 367720",
  "D 266681",
  "L 577262",
  "U 829975",
  "L 112010",
  "D 829975",
  "L 491645",
  "U 686074",
  "L 5411",
  "U 500254"
)
hex_plan <- parse_hex_plan(example)
stopifnot(part_one_solver(example) == "62")
stopifnot(identical(paste(hex_plan$dir, hex_plan$dist), example_hex))
stopifnot(part_two_solver(example) == "952408144115")

res_01 <- part_one_solver(input)
res_02 <- part_two_solver(input)

cat(sprintf("Part 1 answer: %s\n", res_01))
cat(sprintf("Part 2 answer: %s\n", res_02))

plot_path <- save_lagoon_plot(input)
cat(sprintf("Plot: %s\n", plot_path))
