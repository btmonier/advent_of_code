source("src/utils.R")

# --- Day 25: Snowverload ---

input <- read_input("day_25")

# Undirected wiring diagram. Each connection is listed once.
parse_wires <- function(lines) {
  left <- character()
  right <- character()
  for (line in lines) {
    parts <- strsplit(line, ": ", fixed = TRUE)[[1L]]
    others <- strsplit(parts[[2L]], " ", fixed = TRUE)[[1L]]
    left <- c(left, rep(parts[[1L]], length(others)))
    right <- c(right, others)
  }
  nodes <- unique(c(left, right))
  id <- setNames(seq_along(nodes), nodes)
  n <- length(nodes)
  adj <- replicate(n, integer(), simplify = FALSE)
  for (i in seq_along(left)) {
    u <- id[[left[[i]]]]
    v <- id[[right[[i]]]]
    adj[[u]] <- c(adj[[u]], v)
    adj[[v]] <- c(adj[[v]], u)
  }
  adj
}

# Edges that lie on many shortest paths are the bottlenecks. The three wires
# to unplug are among those, and removing them leaves exactly two groups.
group_product <- function(lines, samples = 250L, pool = 12L) {
  adj <- parse_wires(lines)
  n <- length(adj)
  count <- matrix(0L, n, n)
  set.seed(1L)

  for (sample_i in seq_len(samples)) {
    pair <- sample.int(n, 2L)
    start <- pair[[1L]]
    goal <- pair[[2L]]
    parent <- rep(NA_integer_, n)
    parent[[start]] <- 0L
    queue <- integer(n)
    head <- 1L
    tail <- 1L
    queue[[1L]] <- start
    while (head <= tail && is.na(parent[[goal]])) {
      node <- queue[[head]]
      head <- head + 1L
      for (nxt in adj[[node]]) {
        if (is.na(parent[[nxt]])) {
          parent[[nxt]] <- node
          tail <- tail + 1L
          queue[[tail]] <- nxt
        }
      }
    }
    cur <- goal
    while (cur != start) {
      prev <- parent[[cur]]
      a <- min(prev, cur)
      b <- max(prev, cur)
      count[[a, b]] <- count[[a, b]] + 1L
      cur <- prev
    }
  }

  hits <- which(count > 0L, arr.ind = TRUE)
  hits <- hits[order(count[hits], decreasing = TRUE), , drop = FALSE]
  hits <- hits[seq_len(min(pool, nrow(hits))), , drop = FALSE]
  choices <- combn(nrow(hits), 3L)

  for (k in seq_len(ncol(choices))) {
    cuts <- hits[choices[, k], , drop = FALSE]
    graph <- adj
    for (i in seq_len(nrow(cuts))) {
      u <- cuts[[i, 1L]]
      v <- cuts[[i, 2L]]
      graph[[u]] <- graph[[u]][graph[[u]] != v]
      graph[[v]] <- graph[[v]][graph[[v]] != u]
    }
    seen <- rep(FALSE, n)
    sizes <- integer()
    for (start in seq_len(n)) {
      if (seen[[start]]) next
      seen[[start]] <- TRUE
      queue <- integer(n)
      head <- 1L
      tail <- 1L
      queue[[1L]] <- start
      while (head <= tail) {
        node <- queue[[head]]
        head <- head + 1L
        for (nxt in graph[[node]]) {
          if (!seen[[nxt]]) {
            seen[[nxt]] <- TRUE
            tail <- tail + 1L
            queue[[tail]] <- nxt
          }
        }
      }
      sizes <- c(sizes, tail)
    }
    if (length(sizes) == 2L) return(sizes[[1L]] * sizes[[2L]])
  }

  stop("No 3-wire cut found")
}

example <- c(
  "jqt: rhn xhk nvd",
  "rsh: frs pzl lsr",
  "xhk: hfx",
  "cmg: qnr nvd lhk bvb",
  "rhn: xhk bvb hfx",
  "bvb: xhk hfx",
  "pzl: lsr hfx nvd",
  "qnr: nvd",
  "ntq: jqt hfx bvb xhk",
  "nvd: lhk",
  "lsr: lhk",
  "rzs: qnr cmg lsr rsh",
  "frs: qnr lhk lsr"
)
stopifnot(group_product(example) == 54)

res_01 <- group_product(input)

cat(sprintf("Part 1 answer: %s\n", format(res_01, scientific = FALSE)))
