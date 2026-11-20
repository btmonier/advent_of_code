source("src/utils.R")

# --- Day 08: Haunted Wasteland ---

input <- read_input("day_08")

# First line is the repeating L/R instructions. Each later line is one node.
parse_map <- function(lines) {
  parsed <- strcapture(
    "^(\\w+) = \\((\\w+), (\\w+)\\)$",
    lines[-1],
    proto = list(name = "", left = "", right = "")
  )
  list(
    instructions = strsplit(lines[[1]], "")[[1]],
    network = setNames(Map(c, parsed$left, parsed$right), parsed$name)
  )
}

# One step along the instruction cycle. `steps` is how many steps have already been taken.
follow <- function(network, instructions, node, steps) {
  direction <- instructions[[(steps %% length(instructions)) + 1]]
  choices <- network[[node]]
  if (direction == "L") choices[[1]] else choices[[2]]
}

# Follow the instruction cycle from AAA until ZZZ.
part_one_solver <- function(lines) {
  map <- parse_map(lines)
  node <- "AAA"
  steps <- 0
  repeat {
    node <- follow(map$network, map$instructions, node, steps)
    steps <- steps + 1
    if (node == "ZZZ") return(steps)
  }
}

gcd <- function(a, b) {
  while (b != 0) {
    remainder <- a %% b
    a <- b
    b <- remainder
  }
  a
}

lcm <- function(a, b) a / gcd(a, b) * b

# Each ghost starts on a node ending in A. On this map, every such path first
# reaches a node ending in Z after `period` steps and then hits that same node
# every `period` steps, so the ghosts meet at the least common multiple.
part_two_solver <- function(lines) {
  map <- parse_map(lines)
  network <- map$network
  instructions <- map$instructions
  starts <- names(network)[endsWith(names(network), "A")]

  periods <- vapply(starts, function(start) {
    node <- start
    steps <- 0
    repeat {
      node <- follow(network, instructions, node, steps)
      steps <- steps + 1
      if (endsWith(node, "Z")) break
    }
    period <- steps
    arrived <- node
    repeat {
      node <- follow(network, instructions, node, steps)
      steps <- steps + 1
      if (endsWith(node, "Z")) break
    }
    stopifnot(steps == 2 * period, identical(node, arrived))
    period
  }, numeric(1))

  Reduce(lcm, periods)
}

example_short <- c(
  "RL",
  "AAA = (BBB, CCC)",
  "BBB = (DDD, EEE)",
  "CCC = (ZZZ, GGG)",
  "DDD = (DDD, DDD)",
  "EEE = (EEE, EEE)",
  "GGG = (GGG, GGG)",
  "ZZZ = (ZZZ, ZZZ)"
)
example_repeat <- c(
  "LLR",
  "AAA = (BBB, BBB)",
  "BBB = (AAA, ZZZ)",
  "ZZZ = (ZZZ, ZZZ)"
)
example_ghosts <- c(
  "LR",
  "11A = (11B, XXX)",
  "11B = (XXX, 11Z)",
  "11Z = (11B, XXX)",
  "22A = (22B, XXX)",
  "22B = (22C, 22C)",
  "22C = (22Z, 22Z)",
  "22Z = (22B, 22B)",
  "XXX = (XXX, XXX)"
)
stopifnot(part_one_solver(example_short) == 2)
stopifnot(part_one_solver(example_repeat) == 6)
stopifnot(part_two_solver(example_ghosts) == 6)

res_01 <- part_one_solver(input)
res_02 <- part_two_solver(input)

cat(sprintf("Part 1 answer: %s\n", format(res_01, scientific = FALSE)))
cat(sprintf("Part 2 answer: %s\n", format(res_02, scientific = FALSE)))
