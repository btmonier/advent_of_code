package days

import (
	"fmt"
	"os"
)

// Run executes the solution for the given day.
func Run(day int) {
	switch day {
	default:
		fmt.Fprintf(os.Stderr, "Day %d not implemented yet!\n", day)
	}
}
