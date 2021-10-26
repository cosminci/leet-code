package io.github.cosminci.leetcode._100

import scala.collection.mutable

object _15_ThreeSum:
  def main(args: Array[String]): Unit =
    println(threeSum(Array(-1, 0, 1, 2, -1, -4)))

  private def threeSum(n: Array[Int]): List[List[Int]] =
    val results = mutable.Set.empty[List[Int]]
    val nums    = n.sorted
    (0 until nums.length - 2).foreach { i =>
      var j = i + 1
      var k = nums.length - 1
      while j < k do
        if nums(i) + nums(j) + nums(k) == 0 then
          results.addOne(List(nums(i), nums(j), nums(k)))
          j += 1
          k -= 1
        else if nums(i) + nums(j) + nums(k) > 0 then k -= 1
        else j += 1
    }
    results.toList
