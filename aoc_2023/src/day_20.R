source("src/utils.R")

# --- Day 20: Pulse Propagation ---

input <- read_input("day_20")

# Flip-flops start off. Conjunctions remember a low pulse from every input.
parse_modules <- function(lines) {
  modules <- list()
  for (line in lines) {
    parts <- strsplit(line, " -> ", fixed = TRUE)[[1L]]
    raw_name <- parts[[1L]]
    dests <- strsplit(parts[[2L]], ", ", fixed = TRUE)[[1L]]
    if (startsWith(raw_name, "%")) {
      type <- "flip"
      name <- substring(raw_name, 2L)
    } else if (startsWith(raw_name, "&")) {
      type <- "conj"
      name <- substring(raw_name, 2L)
    } else {
      type <- "broadcast"
      name <- raw_name
    }
    modules[[name]] <- list(
      type = type,
      dests = dests,
      on = FALSE,
      memory = list()
    )
  }

  for (name in names(modules)) {
    for (dest in modules[[name]]$dests) {
      if (!is.null(modules[[dest]]) && modules[[dest]]$type == "conj") {
        modules[[dest]]$memory[[name]] <- FALSE
      }
    }
  }
  modules
}

# Pulses are handled in the order they are sent. A high pulse is TRUE.
press_button <- function(modules) {
  low <- 1L
  high <- 0L
  from <- character(4096L)
  to <- character(4096L)
  is_high <- logical(4096L)
  head <- 1L
  tail <- 1L
  to[[1L]] <- "broadcaster"
  is_high[[1L]] <- FALSE

  while (head <= tail) {
    src <- from[[head]]
    dest <- to[[head]]
    pulse <- is_high[[head]]
    head <- head + 1L

    mod <- modules[[dest]]
    if (is.null(mod)) next

    send <- NULL
    if (mod$type == "broadcast") {
      send <- pulse
    } else if (mod$type == "flip") {
      if (!pulse) {
        mod$on <- !mod$on
        modules[[dest]] <- mod
        send <- mod$on
      }
    } else {
      mod$memory[[src]] <- pulse
      modules[[dest]] <- mod
      send <- !all(unlist(mod$memory, use.names = FALSE))
    }

    if (is.null(send)) next
    for (next_dest in mod$dests) {
      tail <- tail + 1L
      if (tail > length(to)) {
        extra <- length(to)
        from <- c(from, character(extra))
        to <- c(to, character(extra))
        is_high <- c(is_high, logical(extra))
      }
      from[[tail]] <- dest
      to[[tail]] <- next_dest
      is_high[[tail]] <- send
      if (send) high <- high + 1L else low <- low + 1L
    }
  }

  list(modules = modules, low = low, high = high)
}

pulse_product <- function(lines, presses = 1000L) {
  modules <- parse_modules(lines)
  low <- 0
  high <- 0
  for (i in seq_len(presses)) {
    result <- press_button(modules)
    modules <- result$modules
    low <- low + result$low
    high <- high + result$high
  }
  low * high
}

example_one <- c(
  "broadcaster -> a, b, c",
  "%a -> b",
  "%b -> c",
  "%c -> inv",
  "&inv -> a"
)
example_two <- c(
  "broadcaster -> a",
  "%a -> inv, con",
  "&inv -> b",
  "%b -> con",
  "&con -> output"
)
stopifnot(pulse_product(example_one) == 32000000)
stopifnot(pulse_product(example_two) == 11687500)

# rx is fed by a single conjunction. That conjunction sends a low pulse only
# when every input's most recent pulse was high. Each input sends that high
# pulse on its own fixed period, as its first pulse of the press, so the
# presses line up at the least common multiple of those periods.
gcd_int <- function(a, b) {
  a <- as.numeric(a)
  b <- as.numeric(b)
  while (b != 0) {
    next_b <- a %% b
    a <- b
    b <- next_b
  }
  a
}

lcm_int <- function(a, b) (as.numeric(a) / gcd_int(a, b)) * as.numeric(b)

# Button presses on which each input first and next sends a high pulse to dest.
high_pulse_periods <- function(modules, dest) {
  inputs <- names(modules[[dest]]$memory)
  first <- setNames(rep(NA_real_, length(inputs)), inputs)
  second <- first
  presses <- 0

  repeat {
    presses <- presses + 1
    from <- character(8192L)
    to <- character(8192L)
    is_high <- logical(8192L)
    head <- 1L
    tail <- 1L
    to[[1L]] <- "broadcaster"

    while (head <= tail) {
      src <- from[[head]]
      pulse_dest <- to[[head]]
      pulse <- is_high[[head]]
      head <- head + 1L

      if (pulse && pulse_dest == dest && src %in% inputs) {
        if (is.na(first[[src]])) {
          first[[src]] <- presses
        } else if (is.na(second[[src]]) && presses != first[[src]]) {
          second[[src]] <- presses
        }
      }

      mod <- modules[[pulse_dest]]
      if (is.null(mod)) next

      send <- NULL
      if (mod$type == "broadcast") {
        send <- pulse
      } else if (mod$type == "flip") {
        if (!pulse) {
          mod$on <- !mod$on
          modules[[pulse_dest]] <- mod
          send <- mod$on
        }
      } else {
        mod$memory[[src]] <- pulse
        modules[[pulse_dest]] <- mod
        send <- !all(unlist(mod$memory, use.names = FALSE))
      }

      if (is.null(send)) next
      for (next_dest in mod$dests) {
        tail <- tail + 1L
        if (tail > length(to)) {
          extra <- length(to)
          from <- c(from, character(extra))
          to <- c(to, character(extra))
          is_high <- c(is_high, logical(extra))
        }
        from[[tail]] <- pulse_dest
        to[[tail]] <- next_dest
        is_high[[tail]] <- send
      }
    }

    if (all(!is.na(second))) break
  }

  stopifnot(all(second == 2 * first))
  first
}

presses_until_rx <- function(lines) {
  modules <- parse_modules(lines)
  feeder <- names(modules)[vapply(modules, function(mod) "rx" %in% mod$dests, logical(1))]
  periods <- high_pulse_periods(modules, feeder)
  Reduce(lcm_int, periods)
}

res_01 <- pulse_product(input)
res_02 <- presses_until_rx(input)

cat(sprintf("Part 1 answer: %s\n", format(res_01, scientific = FALSE)))
cat(sprintf("Part 2 answer: %s\n", format(res_02, scientific = FALSE, trim = TRUE)))
