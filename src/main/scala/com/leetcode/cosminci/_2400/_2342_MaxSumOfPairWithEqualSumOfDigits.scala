package com.leetcode.cosminci._2400

object _2342_MaxSumOfPairWithEqualSumOfDigits:

  def maximumSum(nums: Array[Int]): Int =
    @annotation.tailrec
    def digitSum(n: Int, s: Int): Int =
      if n == 0 then s else digitSum(n / 10, s + n % 10)

    nums.foldLeft(-1, Map.empty[Int, Int]) {
      case ((maxSum, maxNumPerDigitSum), n) =>
        val sum = digitSum(n, 0)
        maxNumPerDigitSum.get(sum) match
          case None      => (maxSum, maxNumPerDigitSum.updated(sum, n))
          case Some(max) => (maxSum.max(max + n), maxNumPerDigitSum.updated(sum, max.max(n)))
      }._1
