source("src/utils.R")

# --- Day 01: Trebuchet?! ---

input <- read_input("day_01")

# Part 1: Extract first and last digit from each line
part_one_solver <- function(lines) {
  lines |>
    lapply(\(line) {
      digits <- suppressWarnings(
        strsplit(line, "") |> unlist() |> as.numeric()
      )
      digits <- digits[!is.na(digits)] |> as.character()
      paste0(digits[1], digits[length(digits)]) |> as.numeric()
    }) |>
    unlist() |>
    sum()
}

res_01 <- part_one_solver(input)

# Part 2: Also parse spelled-out numbers (handling overlaps like "twone")
txt_num_map <- c(
  "one"   = "o1e",
  "two"   = "t2o",
  "three" = "t3e",
  "four"  = "f4r",
  "five"  = "f5e",
  "six"   = "s6x",
  "seven" = "s7n",
  "eight" = "e8t",
  "nine"  = "n9e"
)

part_two_solver <- function(lines) {
  lines |>
    lapply(\(line) {
      for (pattern in names(txt_num_map)) {
        line <- gsub(pattern, txt_num_map[[pattern]], line)
      }
      digits <- suppressWarnings(
        strsplit(line, "") |> unlist() |> as.numeric()
      )
      digits <- digits[!is.na(digits)] |> as.character()
      paste0(digits[1], digits[length(digits)]) |> as.numeric()
    }) |>
    unlist() |>
    sum()
}

res_02 <- part_two_solver(input)

# Output results
cat(sprintf("Part 1 answer: %s\n", res_01))
cat(sprintf("Part 2 answer: %s\n", res_02))

