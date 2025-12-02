
fun xmasAccumulator(aMat: List<CharArray>): Int {
    var xAcc = 0

    for (i in aMat.indices) {
        for (j in aMat[i].indices) {
            if (aMat[i][j] == 'X') {
                // Left
                if (aMat[i].getOrNull(j + 3) != null) {
                    if (aMat[i][j + 1] == 'M' && aMat[i][j + 2] == 'A' && aMat[i][j + 3] == 'S') xAcc++
                }

                // Right
                if (aMat[i].getOrNull(j - 3) != null) {
                    if (aMat[i][j - 1] == 'M' && aMat[i][j - 2] == 'A' && aMat[i][j - 3] == 'S') xAcc++
                }

                // Down
                if (aMat[j].getOrNull(i + 3) != null) {
                    if (aMat[i + 1][j] == 'M' && aMat[i + 2][j] == 'A' && aMat[i + 3][j] == 'S') xAcc++
                }

                // Up
                if (aMat[j].getOrNull(i - 3) != null) {
                    if (aMat[i - 1][j] == 'M' && aMat[i - 2][j] == 'A' && aMat[i - 3][j] == 'S') xAcc++
                }

                // Diag-down-right
                if (aMat[i].getOrNull(j + 3) != null && aMat[j].getOrNull(i + 3) != null) {
                    if (aMat[i + 1][j + 1] == 'M' && aMat[i + 2][j + 2] == 'A' && aMat[i + 3][j + 3] == 'S') xAcc++
                }

                // Diag-up-right
                if (aMat[i].getOrNull(j - 3) != null && aMat[j].getOrNull(i - 3) != null) {
                    if (aMat[i - 1][j - 1] == 'M' && aMat[i - 2][j - 2] == 'A' && aMat[i - 3][j - 3] == 'S') xAcc++
                }

                // Diag-down-left
                if (aMat[i].getOrNull(j - 3) != null && aMat[j].getOrNull(i + 3) != null) {
                    if (aMat[i + 1][j - 1] == 'M' && aMat[i + 2][j - 2] == 'A' && aMat[i + 3][j - 3] == 'S') xAcc++
                }

                // Diag-up-left
                if (aMat[i].getOrNull(j + 3) != null && aMat[j].getOrNull(i - 3) != null) {
                    if (aMat[i - 1][j + 1] == 'M' && aMat[i - 2][j + 2] == 'A' && aMat[i - 3][j + 3] == 'S') xAcc++
                }
            }
        }
    }

    return xAcc
}

fun masAccumulator(aMat: List<CharArray>): Int {
    var xAcc = 0

    val nCols = aMat[0].size
    val nRows = aMat.size

    for (i in aMat.indices) {
        for (j in aMat[i].indices) {
            if (
                aMat.getOrNull(i + 1)?.getOrNull(j + 1) != null &&      // UL
                aMat.getOrNull(i + 1)?.getOrNull(nCols - j) != null &&  // UR
                aMat.getOrNull(nRows - i)?.getOrNull(j + 1) != null &&  // DL
                aMat.getOrNull(nRows - i)?.getOrNull(nCols - 1) != null // DR
            ) {
                if (aMat[i][j] == 'A') {
                    //    * . .                        . . *                        . . .                        . . .
                    //    . . .                        . . .                        . . .                        . . .
                    //    . . .                        . . .                        * . .                        . . *
                    if (aMat[i - 1][j - 1] == 'M' && aMat[i - 1][j + 1] == 'M' && aMat[i + 1][j - 1] == 'S' && aMat[i + 1][j + 1] == 'S') xAcc++
                    if (aMat[i - 1][j - 1] == 'S' && aMat[i - 1][j + 1] == 'S' && aMat[i + 1][j - 1] == 'M' && aMat[i + 1][j + 1] == 'M') xAcc++
                    if (aMat[i - 1][j - 1] == 'S' && aMat[i - 1][j + 1] == 'M' && aMat[i + 1][j - 1] == 'S' && aMat[i + 1][j + 1] == 'M') xAcc++
                    if (aMat[i - 1][j - 1] == 'M' && aMat[i - 1][j + 1] == 'S' && aMat[i + 1][j - 1] == 'M' && aMat[i + 1][j + 1] == 'S') xAcc++
                }
            }
        }
    }

    return xAcc
}

fun main() {
    val dayInput = readInput("day_04").map { it.toCharArray() }

    // Part 1
    val res01 = xmasAccumulator(dayInput)

    // Part 2
    val res02 = masAccumulator(dayInput)

    println("Part 1 answer: $res01")
    println("Part 2 answer: $res02")
}