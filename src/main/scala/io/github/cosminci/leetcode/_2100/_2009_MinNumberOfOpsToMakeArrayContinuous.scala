package io.github.cosminci.leetcode._2100

object _2009_MinNumberOfOpsToMakeArrayContinuous:
  def main(args: Array[String]): Unit =
    println(minOperations(Array(4, 2, 5, 3)))
    println(minOperations(Array(1, 2, 3, 5, 6)))
    println(minOperations(Array(1, 10, 11, 1000)))

  private def minOperations(nums: Array[Int]): Int =
    val unique = nums.distinct.sorted
    val (n, m) = (nums.length, unique.length)

    (0 until m)
      .foldLeft(n, 0) { case ((min, end), start) =>
        val newEnd          = (end until m).find(i => unique(i) >= unique(start) + n).getOrElse(m)
        val alreadyInWindow = newEnd - start
        val swapsNeeded     = n - alreadyInWindow
        (math.min(min, swapsNeeded), newEnd)
      }
      ._1
