package com.leetcode.cosminci._2600

object _2553_SeparateDigitsInArray:

  def separateDigits(nums: Array[Int]): Array[Int] =
    nums.flatMap(_.toString.map(_ - '0'))
