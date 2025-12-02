
fun main() {
    val dayInput = readInput("day_05")

    val rules = dayInput
        .filter { it.contains("|") }
        .map { it.split("|").map { rule -> rule.toInt() }.let { it[0] to it[1] } }

    val updates = dayInput
        .filter { it.contains(",") }
        .map { it.split(",").map { rule -> rule.toInt() } }

    fun missing(left: Int, right: Int) = left to right !in rules

    fun isValid(ints: List<Int>): Boolean {
        for (li in 0..<ints.lastIndex)
            for (ri in li + 1..ints.lastIndex)
                if (missing(ints[li], ints[ri]))
                    return false
        return true
    }

    fun <T> List<T>.middle() = this[if (size % 2 == 1) (size - 1) / 2 else size / 2]

    fun part1() = updates
        .filter(::isValid)
        .sumOf(List<Int>::middle)

    fun part2() = updates.filterNot(::isValid).map(List<Int>::toMutableList).sumOf { update ->
        check@ while (true) {
            for (li in 0..<update.lastIndex) {
                val left = update[li]
                val ri = li + 1
                val right = update[ri]

                if (missing(left, right)) {
                    update[li] = right
                    update[ri] = left
                    continue@check
                }
            }
            break
        }

        update.middle()
    }

    val res01 = part1()
    val res02 = part2()

    println("Part 1 answer: $res01")
    println("Part 2 answer: $res02")
}