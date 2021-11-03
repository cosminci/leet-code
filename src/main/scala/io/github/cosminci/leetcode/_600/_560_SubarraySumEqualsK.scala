package io.github.cosminci.leetcode._600

object _560_SubarraySumEqualsK:
  def main(args: Array[String]): Unit =
    println(subarraySum(Array(1, 2, 3), 3))
    println(subarraySumMap(Array(1, 2, 3), 3))

  def subarraySumMap(nums: Array[Int], k: Int): Int =
    nums
      .foldLeft((Map(0 -> 1), 0, 0)) { case ((cumulativeSumCounts, kCount, cumulativeSum), n) =>
        val newCumulativeSum = cumulativeSum + n
        val newKCount =
          if cumulativeSumCounts.contains(newCumulativeSum - k) then kCount + cumulativeSumCounts(newCumulativeSum - k)
          else kCount
        val newCumulativeSumCounts = cumulativeSumCounts.updatedWith(newCumulativeSum) {
          case None    => Some(1)
          case Some(c) => Some(c + 1)
        }
        (newCumulativeSumCounts, newKCount, newCumulativeSum)
      }
      ._2

  def subarraySum(nums: Array[Int], k: Int): Int =
    var count = 0
    nums.indices.foreach { i =>
      var sum = 0
      (i until nums.length).foreach { j =>
        sum = sum + nums(j)
        if sum == k then count += 1
      }
    }
    count
