fun main() {
    val equations = readInput("day_07").map { line ->
        val (target, rest) = line.split(": ")
        target.toLong() to rest.split(" ").map { it.toLong() }
    }

    fun concat(left: Long, right: Long): Long? {
        var scale = 1L
        var remaining = right
        do {
            if (scale > Long.MAX_VALUE / 10) return null
            scale *= 10
            remaining /= 10
        } while (remaining > 0)
        if (left > (Long.MAX_VALUE - right) / scale) return null
        return left * scale + right
    }

    fun canCalibrate(target: Long, nums: List<Long>, allowConcat: Boolean): Boolean {
        fun search(index: Int, current: Long): Boolean {
            if (current > target) return false
            if (index == nums.size) return current == target
            val next = nums[index]
            if (search(index + 1, current + next)) return true
            if (current == 0L || next <= Long.MAX_VALUE / current) {
                if (search(index + 1, current * next)) return true
            }
            if (!allowConcat) return false
            val concatenated = concat(current, next) ?: return false
            return search(index + 1, concatenated)
        }
        return search(1, nums[0])
    }

    val res01 = equations
        .filter { (target, nums) -> canCalibrate(target, nums, allowConcat = false) }
        .sumOf { it.first }

    val res02 = equations
        .filter { (target, nums) -> canCalibrate(target, nums, allowConcat = true) }
        .sumOf { it.first }

    println("Part 1 answer: $res01")
    println("Part 2 answer: $res02")
}
