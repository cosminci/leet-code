package io.github.cosminci.leetcode._2500

object _2442_CountNumdistinctIntsAfterReverseOps:

  def countDistinctIntegers(nums: Array[Int]): Int =
    @annotation.tailrec
    def reverse(n: Int, r: Int): Int =
      if n == 0 then r else reverse(n / 10, r * 10 + n % 10)

    nums.foldLeft(nums.toSet)((distinct, n) => distinct + reverse(n, r = 0)).size
