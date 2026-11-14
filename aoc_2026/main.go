package main

import (
	"os"
	"strconv"

	"aoc_2026/days"
)

func main() {
	day := 1
	if len(os.Args) > 1 {
		if n, err := strconv.Atoi(os.Args[1]); err == nil {
			day = n
		}
	}

	days.Run(day)
}
