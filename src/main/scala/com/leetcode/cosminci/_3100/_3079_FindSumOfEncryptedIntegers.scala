package com.leetcode.cosminci._3100

object _3079_FindSumOfEncryptedIntegers:

  def sumOfEncryptedInt(nums: Array[Int]): Int =
    nums.map(_.toString).map(n => (n.max.toString * n.length).toInt).sum
