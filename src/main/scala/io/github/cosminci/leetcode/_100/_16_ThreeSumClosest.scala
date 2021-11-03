package io.github.cosminci.leetcode._100

object _16_ThreeSumClosest:

  def main(args: Array[String]): Unit =
    println(threeSumClosest(Array(1, 1, -1, 3, -5, 3, -2, 4, 7, 5, -1), -14))

  def threeSumClosest(n: Array[Int], target: Int): Int =
    var closest = if target < 0 then Int.MinValue else Int.MaxValue
    val nums    = n.sorted
    (0 until nums.length - 2).foreach { i =>
      var j = i + 1
      var k = nums.length - 1
      while j < k do
        val sum = nums(i) + nums(j) + nums(k)
        if math.abs(target - sum) < math.abs(target - closest) then closest = sum
        if sum > target then k -= 1
        else j += 1
    }
    closest
