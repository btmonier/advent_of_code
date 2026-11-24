source("src/utils.R")

# --- Day 15: Lens Library ---

input <- read_input("day_15")

# HASH folds each character into a value from 0 to 255: add its ASCII code,
# multiply by 17, and keep the remainder modulo 256.
hash_string <- function(text) {
  value <- 0L
  for (code in utf8ToInt(text)) {
    value <- ((value + code) * 17L) %% 256L
  }
  value
}

initialization_steps <- function(lines) {
  strsplit(paste(lines, collapse = ""), ",", fixed = TRUE)[[1]]
}

part_one_solver <- function(lines) {
  sum(vapply(initialization_steps(lines), hash_string, integer(1)))
}

# A dash removes the labeled lens. An equals sign replaces that lens's focal
# length, or appends the lens if the box does not already hold that label.
arrange_lenses <- function(steps) {
  boxes <- lapply(seq_len(256L), function(i) {
    list(labels = character(), focals = integer())
  })
  for (step in steps) {
    if (endsWith(step, "-")) {
      label <- substr(step, 1L, nchar(step) - 1L)
      box <- hash_string(label) + 1L
      idx <- match(label, boxes[[box]]$labels)
      if (!is.na(idx)) {
        boxes[[box]]$labels <- boxes[[box]]$labels[-idx]
        boxes[[box]]$focals <- boxes[[box]]$focals[-idx]
      }
    } else {
      parts <- strsplit(step, "=", fixed = TRUE)[[1]]
      label <- parts[[1]]
      focal <- as.integer(parts[[2]])
      box <- hash_string(label) + 1L
      idx <- match(label, boxes[[box]]$labels)
      if (is.na(idx)) {
        boxes[[box]]$labels <- c(boxes[[box]]$labels, label)
        boxes[[box]]$focals <- c(boxes[[box]]$focals, focal)
      } else {
        boxes[[box]]$focals[[idx]] <- focal
      }
    }
  }
  boxes
}

# Focusing power is (box number + 1) * slot * focal length.
focusing_power <- function(boxes) {
  total <- 0
  for (box in seq_along(boxes)) {
    focals <- boxes[[box]]$focals
    if (length(focals) == 0L) next
    total <- total + sum(box * seq_along(focals) * focals)
  }
  total
}

part_two_solver <- function(lines) {
  focusing_power(arrange_lenses(initialization_steps(lines)))
}

example <- "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"
stopifnot(hash_string("HASH") == 52L)
stopifnot(hash_string("rn=1") == 30L)
stopifnot(hash_string("cm-") == 253L)
stopifnot(hash_string("qp=3") == 97L)
stopifnot(hash_string("cm=2") == 47L)
stopifnot(hash_string("qp-") == 14L)
stopifnot(hash_string("pc=4") == 180L)
stopifnot(hash_string("ot=9") == 9L)
stopifnot(hash_string("ab=5") == 197L)
stopifnot(hash_string("pc-") == 48L)
stopifnot(hash_string("pc=6") == 214L)
stopifnot(hash_string("ot=7") == 231L)
stopifnot(part_one_solver(example) == 1320L)
example_boxes <- arrange_lenses(initialization_steps(example))
stopifnot(identical(example_boxes[[1]]$labels, c("rn", "cm")))
stopifnot(identical(example_boxes[[1]]$focals, c(1L, 2L)))
stopifnot(identical(example_boxes[[4]]$labels, c("ot", "ab", "pc")))
stopifnot(identical(example_boxes[[4]]$focals, c(7L, 5L, 6L)))
stopifnot(part_two_solver(example) == 145L)

res_01 <- part_one_solver(input)
res_02 <- part_two_solver(input)

cat(sprintf("Part 1 answer: %s\n", format(res_01, scientific = FALSE)))
cat(sprintf("Part 2 answer: %s\n", format(res_02, scientific = FALSE)))
