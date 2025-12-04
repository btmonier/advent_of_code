fun main() {
    val dayInput = readInput("day_06")

    // Parse the grid
    val grid = dayInput.map { it.toCharArray() }
    val rows = grid.size
    val cols = grid[0].size

    // Find the guard's starting position and direction
    var guardRow = 0
    var guardCol = 0
    var direction = 0 // 0=up, 1=right, 2=down, 3=left

    for (r in 0 until rows) {
        for (c in 0 until cols) {
            when (grid[r][c]) {
                '^' -> { guardRow = r; guardCol = c; direction = 0 }
                '>' -> { guardRow = r; guardCol = c; direction = 1 }
                'v' -> { guardRow = r; guardCol = c; direction = 2 }
                '<' -> { guardRow = r; guardCol = c; direction = 3 }
            }
        }
    }

    // Direction deltas: up, right, down, left
    val dr = intArrayOf(-1, 0, 1, 0)
    val dc = intArrayOf(0, 1, 0, -1)

    // Part 1: Simulate the guard's path and count distinct positions
    val visited = mutableSetOf<Pair<Int, Int>>()
    var r = guardRow
    var c = guardCol
    var d = direction

    while (r in 0 until rows && c in 0 until cols) {
        visited.add(r to c)

        // Check what's in front
        val nextR = r + dr[d]
        val nextC = c + dc[d]

        if (nextR !in 0 until rows || nextC !in 0 until cols) {
            // Guard will leave the grid
            break
        }

        if (grid[nextR][nextC] == '#') {
            // Turn right 90 degrees
            d = (d + 1) % 4
        } else {
            // Move forward
            r = nextR
            c = nextC
        }
    }

    val res01 = visited.size

    // Part 2: Find positions where adding an obstacle causes a loop
    // Only need to check positions on the guard's original path (excluding start)
    val candidatePositions = visited.filter { it != (guardRow to guardCol) }

    var loopCount = 0
    for ((obsR, obsC) in candidatePositions) {
        if (causesLoop(grid, rows, cols, guardRow, guardCol, direction, obsR, obsC, dr, dc)) {
            loopCount++
        }
    }

    val res02 = loopCount

    println("Part 1 answer: $res01")
    println("Part 2 answer: $res02")
}

/**
 * Check if placing an obstacle at (obsR, obsC) causes the guard to enter a loop.
 * A loop is detected when the guard visits the same position with the same direction twice.
 */
fun causesLoop(
    grid: List<CharArray>,
    rows: Int,
    cols: Int,
    startRow: Int,
    startCol: Int,
    startDir: Int,
    obsR: Int,
    obsC: Int,
    dr: IntArray,
    dc: IntArray
): Boolean {
    val visitedStates = mutableSetOf<Triple<Int, Int, Int>>()
    var r = startRow
    var c = startCol
    var d = startDir

    while (r in 0 until rows && c in 0 until cols) {
        val state = Triple(r, c, d)
        if (state in visitedStates) {
            // Loop detected
            return true
        }
        visitedStates.add(state)

        // Check what's in front
        val nextR = r + dr[d]
        val nextC = c + dc[d]

        if (nextR !in 0 until rows || nextC !in 0 until cols) {
            // Guard will leave the grid - no loop
            return false
        }

        // Check for obstacle (original or new)
        if (grid[nextR][nextC] == '#' || (nextR == obsR && nextC == obsC)) {
            // Turn right 90 degrees
            d = (d + 1) % 4
        } else {
            // Move forward
            r = nextR
            c = nextC
        }
    }

    return false
}

