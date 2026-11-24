source("src/utils.R")

# --- Day 13: Point of Incidence ---

input <- read_input("day_13", drop_empty = FALSE)

# Blank lines separate patterns. A trailing blank line does not make a pattern.
parse_patterns <- function(lines) {
  patterns <- list()
  current <- character()
  for (line in lines) {
    if (!nzchar(line)) {
      if (length(current) > 0L) {
        patterns[[length(patterns) + 1L]] <- current
        current <- character()
      }
    } else {
      current <- c(current, line)
    }
  }
  if (length(current) > 0L) patterns[[length(patterns) + 1L]] <- current
  patterns
}

cell_differences <- function(a, b) {
  sum(strsplit(a, "", fixed = TRUE)[[1]] != strsplit(b, "", fixed = TRUE)[[1]])
}

# Cells that disagree across the line between `after` and `after + 1`. Rows with
# no partner are ignored. A perfect mirror has zero differences; a smudge has one.
reflection_smudges <- function(rows, after) {
  above <- after
  below <- after + 1L
  n <- length(rows)
  smudges <- 0L
  while (above >= 1L && below <= n) {
    smudges <- smudges + cell_differences(rows[[above]], rows[[below]])
    if (smudges > 1L) return(smudges)
    above <- above - 1L
    below <- below + 1L
  }
  smudges
}

column_rows <- function(rows) {
  chars <- strsplit(rows, "", fixed = TRUE)
  vapply(seq_len(nchar(rows[[1]])), function(col) {
    paste(vapply(chars, `[[`, character(1), col), collapse = "")
  }, character(1))
}

# Vertical reflections contribute the columns to their left. Horizontal
# reflections contribute 100 times the rows above them. `smudges` is 0 for a
# perfect mirror and 1 for the line that becomes perfect after fixing one cell.
pattern_score <- function(rows, smudges) {
  found <- integer()
  for (after in seq_len(length(rows) - 1L)) {
    if (reflection_smudges(rows, after) == smudges) found <- c(found, 100L * after)
  }
  cols <- column_rows(rows)
  for (after in seq_len(length(cols) - 1L)) {
    if (reflection_smudges(cols, after) == smudges) found <- c(found, after)
  }
  stopifnot(length(found) == 1L)
  found[[1]]
}

part_one_solver <- function(lines) {
  sum(vapply(parse_patterns(lines), pattern_score, integer(1), smudges = 0L))
}

part_two_solver <- function(lines) {
  sum(vapply(parse_patterns(lines), pattern_score, integer(1), smudges = 1L))
}

example <- c(
  "#.##..##.",
  "..#.##.#.",
  "##......#",
  "##......#",
  "..#.##.#.",
  "..##..##.",
  "#.#.##.#.",
  "",
  "#...##..#",
  "#....#..#",
  "..##..###",
  "#####.##.",
  "#####.##.",
  "..##..###",
  "#....#..#"
)
stopifnot(part_one_solver(example) == 405)
stopifnot(part_two_solver(example) == 400)

res_01 <- part_one_solver(input)
res_02 <- part_two_solver(input)

cat(sprintf("Part 1 answer: %s\n", format(res_01, scientific = FALSE)))
cat(sprintf("Part 2 answer: %s\n", format(res_02, scientific = FALSE)))
