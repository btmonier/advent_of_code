#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
day <- if (length(args) > 0) as.integer(args[1]) else 1

day_file <- sprintf("src/day_%02d.R", day)

if (file.exists(day_file)) {
  source(day_file)
} else {
  cat(sprintf("Day %d not implemented yet!\n", day))
}


