source("src/utils.R")

# --- Day 19: Aplenty ---

input <- read_input("day_19", drop_empty = FALSE)

# Workflows come first, then a blank line, then the parts to sort.
split_input <- function(lines) {
  blank <- which(!nzchar(lines))[[1L]]
  list(
    workflows = lines[seq_len(blank - 1L)],
    parts = lines[nzchar(lines) & seq_along(lines) > blank]
  )
}

parse_workflows <- function(lines) {
  workflows <- list()
  for (line in lines) {
    name <- sub("\\{.*", "", line)
    body <- sub(".*\\{(.*)\\}", "\\1", line)
    workflows[[name]] <- lapply(strsplit(body, ",", fixed = TRUE)[[1L]], function(rule) {
      if (!grepl(":", rule, fixed = TRUE)) return(list(dest = rule))
      bits <- strsplit(rule, ":", fixed = TRUE)[[1L]]
      cond <- bits[[1L]]
      list(
        cat = substr(cond, 1L, 1L),
        op = substr(cond, 2L, 2L),
        value = as.integer(substr(cond, 3L, nchar(cond))),
        dest = bits[[2L]]
      )
    })
  }
  workflows
}

parse_parts <- function(lines) {
  lapply(lines, function(line) {
    nums <- as.integer(regmatches(line, gregexpr("[0-9]+", line))[[1L]])
    c(x = nums[[1L]], m = nums[[2L]], a = nums[[3L]], s = nums[[4L]])
  })
}

# Parts start at workflow "in". The first matching rule sends the part onward.
# "A" accepts it and "R" rejects it.
is_accepted <- function(part, workflows) {
  name <- "in"
  repeat {
    if (name == "A") return(TRUE)
    if (name == "R") return(FALSE)
    for (rule in workflows[[name]]) {
      if (is.null(rule$cat)) {
        name <- rule$dest
        break
      }
      rating <- part[[rule$cat]]
      matches <- if (rule$op == ">") rating > rule$value else rating < rule$value
      if (matches) {
        name <- rule$dest
        break
      }
    }
  }
}

part_one_solver <- function(lines) {
  sections <- split_input(lines)
  workflows <- parse_workflows(sections$workflows)
  parts <- parse_parts(sections$parts)
  accepted <- parts[vapply(parts, is_accepted, logical(1), workflows = workflows)]
  sum(vapply(accepted, sum, numeric(1)))
}

# Every rating runs from 1 to 4000. A condition splits that interval into the
# values that take the rule and the values that fall through to the next one.
count_accepted <- function(workflows) {
  full <- c(1, 4000)

  combinations <- function(ranges) {
    prod(vapply(ranges, function(bounds) bounds[[2]] - bounds[[1]] + 1, numeric(1)))
  }

  walk <- function(name, ranges) {
    if (name == "R") return(0)
    if (name == "A") return(combinations(ranges))
    total <- 0
    for (rule in workflows[[name]]) {
      if (is.null(rule$cat)) {
        total <- total + walk(rule$dest, ranges)
        break
      }
      bounds <- ranges[[rule$cat]]
      lo <- bounds[[1]]
      hi <- bounds[[2]]
      if (rule$op == ">") {
        match <- c(max(lo, rule$value + 1), hi)
        fail <- c(lo, min(hi, rule$value))
      } else {
        match <- c(lo, min(hi, rule$value - 1))
        fail <- c(max(lo, rule$value), hi)
      }
      if (match[[1]] <= match[[2]]) {
        matched <- ranges
        matched[[rule$cat]] <- match
        total <- total + walk(rule$dest, matched)
      }
      if (fail[[1]] > fail[[2]]) break
      ranges[[rule$cat]] <- fail
    }
    total
  }

  walk("in", list(x = full, m = full, a = full, s = full))
}

part_two_solver <- function(lines) {
  count_accepted(parse_workflows(split_input(lines)$workflows))
}

example <- c(
  "px{a<2006:qkq,m>2090:A,rfg}",
  "pv{a>1716:R,A}",
  "lnx{m>1548:A,A}",
  "rfg{s<537:gd,x>2440:R,A}",
  "qs{s>3448:A,lnx}",
  "qkq{x<1416:A,crn}",
  "crn{x>2662:A,R}",
  "in{s<1351:px,qqz}",
  "qqz{s>2770:qs,m<1801:hdj,R}",
  "gd{a>3333:R,R}",
  "hdj{m>838:A,pv}",
  "",
  "{x=787,m=2655,a=1222,s=2876}",
  "{x=1679,m=44,a=2067,s=496}",
  "{x=2036,m=264,a=79,s=2244}",
  "{x=2461,m=1339,a=466,s=291}",
  "{x=2127,m=1623,a=2188,s=1013}"
)
stopifnot(part_one_solver(example) == 19114)
stopifnot(part_two_solver(example) == 167409079868000)

res_01 <- part_one_solver(input)
res_02 <- part_two_solver(input)

cat(sprintf("Part 1 answer: %s\n", format(res_01, scientific = FALSE)))
cat(sprintf("Part 2 answer: %s\n", format(res_02, scientific = FALSE, trim = TRUE)))
