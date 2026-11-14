package aoc

import (
	"fmt"
	"os"
	"strings"
)

// ReadInput reads lines from the given input txt file.
func ReadInput(name string) []string {
	raw := ReadInputRaw(name)
	if raw == "" {
		return []string{}
	}
	return strings.Split(raw, "\n")
}

// ReadInputRaw reads the entire input file as a single string.
func ReadInputRaw(name string) string {
	path := fmt.Sprintf("../input/2026/%s.txt", name)
	data, err := os.ReadFile(path)
	if err != nil {
		panic(fmt.Sprintf("Could not read file: %s", path))
	}
	text := strings.ReplaceAll(string(data), "\r\n", "\n")
	return strings.TrimSpace(text)
}
