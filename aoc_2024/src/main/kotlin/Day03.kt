
fun mulExtract(s: String): List<Pair<Int, Int>> {
    val regex = """mul\((\d{1,3}),(\d{1,3})\)""".toRegex()

    val match = regex.findAll(s)
        .map { match ->
            val (n1, n2) = match.destructured
            n1.toInt() to n2.toInt()
        }
        .toList()

    return match
}

fun findCompatMul(s: String): List<String> {
    val regex = """(?:^|do\(\))(.*?)(?:don't\(\)|$)""".toRegex()

    val match = regex.findAll(s)
        .map { match -> match.groupValues[0] }
        .toList()

    return match
}

fun main() {

    val dayInput = readInput("day_03").joinToString("")

    // Part 1
    val res01 = mulExtract(dayInput).sumOf { it.first * it.second }

    // Part 2
    val res02 = findCompatMul(dayInput).sumOf { mulExtract(it).sumOf { it.first * it.second } }


    println("Part 1 answer: $res01")
    println("Part 2 answer: $res02")
}