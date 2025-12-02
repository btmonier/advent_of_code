import kotlin.math.absoluteValue

fun isSafeReport(x: List<Int>): Boolean {
    val bAsc = x.zipWithNext { a, b -> a <= b }.all { it }
    val bDsc = x.zipWithNext { a, b -> a >= b }.all { it }
    val bRng = x.zipWithNext { a, b -> (a - b).absoluteValue in 1..3 }.all { it }

    return bRng && (bAsc || bDsc)
}

fun main() {
    val dayInput = readInput("day_02")
        .map { lines ->
            lines.split(" ").map { it.toInt() }
        }

    // Part 1
    val res01 = dayInput.count { isSafeReport(it) }

    // Part 2
    val res02 = dayInput
        .count { numbers ->
            numbers.indices.any {
                val skipped = numbers.toMutableList().apply { removeAt(it) }
                isSafeReport(skipped)
            }
        }

    println("Part 1 answer: $res01")
    println("Part 2 answer: $res02")
}