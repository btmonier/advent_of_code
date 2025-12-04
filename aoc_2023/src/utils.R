#' Read input file for a given day
#'
#' @param day_name Name of the day file (e.g., "day_01")
#' @param test Whether to read test input
#' @param trim Whether to trim whitespace from each line (default: TRUE)
#' @param drop_empty Whether to remove empty lines (default: TRUE)
#' @return Character vector of lines
read_input <- function(day_name, test = FALSE, trim = TRUE, drop_empty = TRUE) {
  suffix <- if (test) "_test" else ""
  path <- file.path("..", "input", "2023", paste0(day_name, suffix, ".txt"))
  if (!file.exists(path)) {
    stop("Input file not found: ", path)
  }
  lines <- readLines(path, warn = FALSE)
  if (trim) lines <- trimws(lines)
  if (drop_empty) lines <- lines[nzchar(lines)]
  lines
}

