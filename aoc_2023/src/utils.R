#' Read input file for a given day
#'
#' @param day_name Name of the day file (e.g., "day_01")
#' @param test Whether to read test input
#' @return Character vector of lines
read_input <- function(day_name, test = FALSE) {
  suffix <- if (test) "_test" else ""
  path <- file.path("..", "input", "2023", paste0(day_name, suffix, ".txt"))
  readLines(path, warn = FALSE)
}

