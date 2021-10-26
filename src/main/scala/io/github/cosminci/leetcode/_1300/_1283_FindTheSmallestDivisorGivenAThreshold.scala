package io.github.cosminci.leetcode._1300

object _1283_FindTheSmallestDivisorGivenAThreshold:
  def main(args: Array[String]): Unit =
    println(smallestDivisor(Array(1, 2, 5, 9), 6))
    println(smallestDivisor(Array(44, 22, 33, 11, 1), 5))
    println(smallestDivisor(Array(21212, 10101, 12121), 1000000))
    println(smallestDivisor(Array(2, 3, 4, 7, 11), 11))

  private def smallestDivisor(nums: Array[Int], threshold: Int): Int =
    def withinThreshold(divisor: Int) =
      nums.map(n => math.ceil(n.toDouble / divisor)).sum.toInt <= threshold

    var (l, r) = (1, nums.max)
    while l < r do
      val mid = l + (r - l) / 2
      if withinThreshold(mid) then r = mid
      else l = mid + 1
    l
