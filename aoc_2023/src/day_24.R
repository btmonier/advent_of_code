source("src/utils.R")

# --- Day 24: Never Tell Me The Odds ---

input <- read_input("day_24")

# Positions are hundreds of trillions, so the cross-multiplication products do
# not fit exactly in a double. Base-1e6 limbs keep every comparison exact.
BASE <- 1000000

bi_trim <- function(digits) {
  while (length(digits) > 1L && digits[[length(digits)]] == 0) {
    digits <- digits[-length(digits)]
  }
  digits
}

bi_norm <- function(digits) {
  i <- 1L
  while (i <= length(digits)) {
    if (digits[[i]] >= BASE) {
      carry <- digits[[i]] %/% BASE
      digits[[i]] <- digits[[i]] %% BASE
      if (i == length(digits)) digits <- c(digits, 0)
      digits[[i + 1L]] <- digits[[i + 1L]] + carry
    }
    i <- i + 1L
  }
  bi_trim(digits)
}

as_bi <- function(token) {
  if (startsWith(token, "-")) {
    sign <- -1
    token <- substring(token, 2L)
  } else {
    sign <- 1
  }
  token <- sub("^0+", "", token)
  if (!nzchar(token)) return(list(sign = 1, digits = 0))
  chars <- rev(strsplit(token, "", fixed = TRUE)[[1L]])
  digits <- numeric(0)
  for (start in seq(1L, length(chars), by = 6L)) {
    chunk <- paste(rev(chars[start:min(start + 5L, length(chars))]), collapse = "")
    digits <- c(digits, as.numeric(chunk))
  }
  list(sign = sign, digits = digits)
}

bi_abs_cmp <- function(a, b) {
  if (length(a) != length(b)) return(sign(length(a) - length(b)))
  for (i in seq(length(a), 1L)) {
    if (a[[i]] != b[[i]]) return(sign(a[[i]] - b[[i]]))
  }
  0
}

bi_cmp <- function(x, y) {
  x_zero <- length(x$digits) == 1L && x$digits[[1L]] == 0
  y_zero <- length(y$digits) == 1L && y$digits[[1L]] == 0
  if (x_zero && y_zero) return(0)
  if (x_zero) return(-y$sign)
  if (y_zero) return(x$sign)
  if (x$sign != y$sign) return(if (x$sign > y$sign) 1 else -1)
  x$sign * bi_abs_cmp(x$digits, y$digits)
}

bi_add_abs <- function(a, b) {
  n <- max(length(a), length(b))
  a <- c(a, numeric(n - length(a)))
  b <- c(b, numeric(n - length(b)))
  out <- numeric(n)
  carry <- 0
  for (i in seq_len(n)) {
    cur <- a[[i]] + b[[i]] + carry
    out[[i]] <- cur %% BASE
    carry <- cur %/% BASE
  }
  if (carry > 0) out <- c(out, carry)
  out
}

bi_sub_abs <- function(a, b) {
  b <- c(b, numeric(length(a) - length(b)))
  for (i in seq_along(a)) {
    a[[i]] <- a[[i]] - b[[i]]
    if (a[[i]] < 0) {
      a[[i]] <- a[[i]] + BASE
      a[[i + 1L]] <- a[[i + 1L]] - 1
    }
  }
  bi_trim(a)
}

bi_add <- function(x, y) {
  if (x$sign == y$sign) {
    digits <- bi_add_abs(x$digits, y$digits)
    sign <- if (length(digits) == 1L && digits[[1L]] == 0) 1 else x$sign
    return(list(sign = sign, digits = digits))
  }
  cmp <- bi_abs_cmp(x$digits, y$digits)
  if (cmp == 0) return(list(sign = 1, digits = 0))
  if (cmp > 0) return(list(sign = x$sign, digits = bi_sub_abs(x$digits, y$digits)))
  list(sign = y$sign, digits = bi_sub_abs(y$digits, x$digits))
}

bi_neg <- function(x) {
  if (length(x$digits) == 1L && x$digits[[1L]] == 0) return(list(sign = 1, digits = 0))
  list(sign = -x$sign, digits = x$digits)
}

bi_sub <- function(x, y) bi_add(x, bi_neg(y))

bi_mul <- function(x, y) {
  if ((length(x$digits) == 1L && x$digits[[1L]] == 0) ||
      (length(y$digits) == 1L && y$digits[[1L]] == 0)) {
    return(list(sign = 1, digits = 0))
  }
  out <- numeric(length(x$digits) + length(y$digits))
  for (i in seq_along(x$digits)) {
    carry <- 0
    for (j in seq_along(y$digits)) {
      k <- i + j - 1L
      cur <- out[[k]] + x$digits[[i]] * y$digits[[j]] + carry
      out[[k]] <- cur %% BASE
      carry <- cur %/% BASE
    }
    k <- i + length(y$digits)
    while (carry > 0) {
      if (k > length(out)) out <- c(out, 0)
      cur <- out[[k]] + carry
      out[[k]] <- cur %% BASE
      carry <- cur %/% BASE
      k <- k + 1L
    }
  }
  list(sign = x$sign * y$sign, digits = bi_trim(out))
}

bi_between <- function(value, left, right) {
  if (bi_cmp(left, right) > 0) {
    tmp <- left
    left <- right
    right <- tmp
  }
  bi_cmp(value, left) >= 0 && bi_cmp(value, right) <= 0
}

ZERO <- list(sign = 1, digits = 0)
ONE <- list(sign = 1, digits = 1)

bi_is_zero <- function(x) length(x$digits) == 1L && x$digits[[1L]] == 0

bi_abs <- function(x) list(sign = 1, digits = x$digits)

bi_mul_small <- function(x, k) {
  if (k == 0 || bi_is_zero(x)) return(ZERO)
  sign <- if (k < 0) -x$sign else x$sign
  k <- abs(k)
  digits <- x$digits
  carry <- 0
  for (i in seq_along(digits)) {
    cur <- digits[[i]] * k + carry
    digits[[i]] <- cur %% BASE
    carry <- cur %/% BASE
  }
  while (carry > 0) {
    digits <- c(digits, carry %% BASE)
    carry <- carry %/% BASE
  }
  if (sign < 0 && length(digits) == 1L && digits[[1L]] == 0) sign <- 1
  list(sign = sign, digits = digits)
}

# Divide a non-negative integer by 2. `bit` is the remainder.
bi_shr1 <- function(x) {
  if (bi_is_zero(x)) return(list(q = ZERO, bit = 0))
  digits <- x$digits
  carry <- 0
  for (i in seq(length(digits), 1L)) {
    cur <- digits[[i]] + carry * BASE
    digits[[i]] <- cur %/% 2
    carry <- cur %% 2
  }
  list(q = list(sign = 1, digits = bi_trim(digits)), bit = carry)
}

bi_divmod <- function(n, d) {
  n <- bi_abs(n)
  d <- bi_abs(d)
  if (bi_is_zero(n)) return(list(q = ZERO, r = ZERO))
  if (bi_cmp(n, d) < 0) return(list(q = ZERO, r = n))
  bits <- integer()
  cur <- n
  while (!bi_is_zero(cur)) {
    shr <- bi_shr1(cur)
    bits <- c(shr$bit, bits)
    cur <- shr$q
  }
  q <- ZERO
  r <- ZERO
  for (bit in bits) {
    r <- bi_mul_small(r, 2)
    if (bit == 1) r <- bi_add(r, ONE)
    q <- bi_mul_small(q, 2)
    if (bi_cmp(r, d) >= 0) {
      r <- bi_sub(r, d)
      q <- bi_add(q, ONE)
    }
  }
  list(q = q, r = r)
}

bi_quo <- function(n, d) {
  sign <- n$sign * d$sign
  q <- bi_divmod(n, d)$q
  if (bi_is_zero(q) || sign > 0) return(q)
  q$sign <- -1
  q
}

bi_gcd <- function(x, y) {
  x <- bi_abs(x)
  y <- bi_abs(y)
  while (!bi_is_zero(y)) {
    next_y <- bi_divmod(x, y)$r
    x <- y
    y <- next_y
  }
  x
}

bi_to_string <- function(x) {
  if (bi_is_zero(x)) return("0")
  ten <- list(sign = 1, digits = 10)
  chars <- character()
  n <- bi_abs(x)
  while (!bi_is_zero(n)) {
    dm <- bi_divmod(n, ten)
    chars <- c(as.character(dm$r$digits[[1L]]), chars)
    n <- dm$q
  }
  paste0(if (x$sign < 0) "-" else "", paste(chars, collapse = ""))
}

rat_make <- function(n, d) {
  if (bi_is_zero(n)) return(list(n = ZERO, d = ONE))
  if (d$sign < 0) {
    n <- bi_neg(n)
    d <- bi_neg(d)
  }
  g <- bi_gcd(n, d)
  list(n = bi_quo(n, g), d = bi_quo(d, g))
}

as_rat <- function(x) {
  if (bi_is_zero(x)) return(list(n = ZERO, d = ONE))
  list(n = x, d = ONE)
}

rat_is_zero <- function(x) bi_is_zero(x$n)

rat_add <- function(x, y) {
  rat_make(bi_add(bi_mul(x$n, y$d), bi_mul(y$n, x$d)), bi_mul(x$d, y$d))
}

rat_sub <- function(x, y) rat_add(x, list(n = bi_neg(y$n), d = y$d))

rat_mul <- function(x, y) rat_make(bi_mul(x$n, y$n), bi_mul(x$d, y$d))

rat_div <- function(x, y) rat_make(bi_mul(x$n, y$d), bi_mul(x$d, y$n))

bi_cross <- function(a, b) {
  list(
    bi_sub(bi_mul(a[[2L]], b[[3L]]), bi_mul(a[[3L]], b[[2L]])),
    bi_sub(bi_mul(a[[3L]], b[[1L]]), bi_mul(a[[1L]], b[[3L]])),
    bi_sub(bi_mul(a[[1L]], b[[2L]]), bi_mul(a[[2L]], b[[1L]]))
  )
}

parse_hail <- function(lines) {
  lapply(lines, function(line) {
    lapply(regmatches(line, gregexpr("-?\\d+", line))[[1L]], as_bi)
  })
}

# Ignore z. Paths cross in the future when both times are non-negative and the
# crossing lies inside the test square.
future_crossings <- function(lines, lo, hi) {
  hail <- parse_hail(lines)
  lo <- as_bi(lo)
  hi <- as_bi(hi)
  total <- 0L
  n <- length(hail)

  for (i in seq_len(n - 1L)) {
    a <- hail[[i]]
    for (j in (i + 1L):n) {
      b <- hail[[j]]
      det <- bi_sub(bi_mul(a[[4L]], b[[5L]]), bi_mul(a[[5L]], b[[4L]]))
      det_sign <- bi_cmp(det, ZERO)
      if (det_sign == 0) next

      dx <- bi_sub(b[[1L]], a[[1L]])
      dy <- bi_sub(b[[2L]], a[[2L]])
      time_a <- bi_sub(bi_mul(dx, b[[5L]]), bi_mul(dy, b[[4L]]))
      time_b <- bi_sub(bi_mul(dx, a[[5L]]), bi_mul(dy, a[[4L]]))
      if (det_sign > 0) {
        if (bi_cmp(time_a, ZERO) < 0 || bi_cmp(time_b, ZERO) < 0) next
      } else if (bi_cmp(time_a, ZERO) > 0 || bi_cmp(time_b, ZERO) > 0) {
        next
      }

      cross_x <- bi_mul(a[[4L]], time_a)
      cross_y <- bi_mul(a[[5L]], time_a)
      x_lo <- bi_mul(bi_sub(lo, a[[1L]]), det)
      x_hi <- bi_mul(bi_sub(hi, a[[1L]]), det)
      y_lo <- bi_mul(bi_sub(lo, a[[2L]]), det)
      y_hi <- bi_mul(bi_sub(hi, a[[2L]]), det)
      if (bi_between(cross_x, x_lo, x_hi) && bi_between(cross_y, y_lo, y_hi)) {
        total <- total + 1L
      }
    }
  }
  total
}

# At the collision, the rock's offset from a hailstone is parallel to their
# relative velocity. Subtracting that condition for two hailstones cancels the
# quadratic terms and leaves a linear equation in the rock's position and velocity.
rock_position_sum <- function(lines) {
  hail <- parse_hail(lines)
  p0 <- hail[[1L]][1:3]
  v0 <- hail[[1L]][4:6]
  rows <- list()
  for (j in 2:4) {
    pj <- hail[[j]][1:3]
    vj <- hail[[j]][4:6]
    ax <- bi_sub(v0[[1L]], vj[[1L]])
    ay <- bi_sub(v0[[2L]], vj[[2L]])
    az <- bi_sub(v0[[3L]], vj[[3L]])
    bx <- bi_sub(p0[[1L]], pj[[1L]])
    by <- bi_sub(p0[[2L]], pj[[2L]])
    bz <- bi_sub(p0[[3L]], pj[[3L]])
    rhs <- Map(bi_sub, bi_cross(p0, v0), bi_cross(pj, vj))
    rows[[length(rows) + 1L]] <- list(
      as_rat(ZERO), as_rat(az), as_rat(bi_neg(ay)),
      as_rat(ZERO), as_rat(bi_neg(bz)), as_rat(by), as_rat(rhs[[1L]])
    )
    rows[[length(rows) + 1L]] <- list(
      as_rat(bi_neg(az)), as_rat(ZERO), as_rat(ax),
      as_rat(bz), as_rat(ZERO), as_rat(bi_neg(bx)), as_rat(rhs[[2L]])
    )
    rows[[length(rows) + 1L]] <- list(
      as_rat(ay), as_rat(bi_neg(ax)), as_rat(ZERO),
      as_rat(bi_neg(by)), as_rat(bx), as_rat(ZERO), as_rat(rhs[[3L]])
    )
  }

  nvars <- 6L
  pivot_at <- rep(NA_integer_, nvars)
  lead <- 1L
  for (col in seq_len(nvars)) {
    piv <- NA_integer_
    for (r in lead:length(rows)) {
      if (!rat_is_zero(rows[[r]][[col]])) {
        piv <- r
        break
      }
    }
    if (is.na(piv)) next
    tmp <- rows[[lead]]
    rows[[lead]] <- rows[[piv]]
    rows[[piv]] <- tmp
    pivv <- rows[[lead]][[col]]
    for (c in col:(nvars + 1L)) rows[[lead]][[c]] <- rat_div(rows[[lead]][[c]], pivv)
    for (r in seq_along(rows)) {
      if (r == lead || rat_is_zero(rows[[r]][[col]])) next
      factor <- rows[[r]][[col]]
      for (c in col:(nvars + 1L)) {
        rows[[r]][[c]] <- rat_sub(rows[[r]][[c]], rat_mul(factor, rows[[lead]][[c]]))
      }
    }
    pivot_at[[col]] <- lead
    lead <- lead + 1L
  }

  coords <- lapply(seq_len(3L), function(col) {
    value <- rows[[pivot_at[[col]]]][[nvars + 1L]]
    stopifnot(bi_cmp(value$d, ONE) == 0)
    value$n
  })
  bi_add(bi_add(coords[[1L]], coords[[2L]]), coords[[3L]])
}

example <- c(
  "19, 13, 30 @ -2,  1, -2",
  "18, 19, 22 @ -1, -1, -2",
  "20, 25, 34 @ -2, -2, -4",
  "12, 31, 28 @ -1, -2, -1",
  "20, 19, 15 @  1, -5, -3"
)
stopifnot(future_crossings(example, "7", "27") == 2)
stopifnot(bi_to_string(rock_position_sum(example)) == "47")

res_01 <- future_crossings(input, "200000000000000", "400000000000000")
res_02 <- bi_to_string(rock_position_sum(input))

cat(sprintf("Part 1 answer: %s\n", format(res_01, scientific = FALSE)))
cat(sprintf("Part 2 answer: %s\n", res_02))
