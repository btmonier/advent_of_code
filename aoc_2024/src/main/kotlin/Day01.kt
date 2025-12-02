import kotlin.math.absoluteValue

fun main() {

    val dayInput = readInput("day_01")

    // Part 1
    val res01 = dayInput
        .map { line ->
            line
                .split("\\s+".toRegex())
                .map { it.toInt() }
                .let { it[0] to it[1] }
        }
        .unzip()
        .let { it.first.sorted() to it.second.sorted() }
        .let { it.first.zip(it.second).map { (it.first - it.second).absoluteValue }}
        .sum()


    // Part 2
    val p2 = dayInput
        .map { line ->
            line.split("\\s+".toRegex())
                .map { it.toInt() }
                .let { it[0] to it[1] }
        }
        .unzip()
        .let {
            it.first to it.second
        }

    val p2Freq = p2.second.groupingBy { it }.eachCount()

    val res02 = p2.first.sumOf {
        val occ = p2Freq[it] ?: 0
        it * occ
    }

    println("Part 1 answer: $res01")
    println("Part 2 answer: $res02")
}