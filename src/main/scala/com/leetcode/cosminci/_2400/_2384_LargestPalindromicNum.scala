package com.leetcode.cosminci._2400

object _2384_LargestPalindromicNum:

  def largestPalindromic(num: String): String =
    val counts = num.groupMapReduce(identity)(_ => 1)(_ + _).withDefaultValue(0)

    val center = "9876543210".find(digit => counts(digit) % 2 == 1)
    val leftHalf = "9876543210"
      .flatMap(digit => digit.toString * (counts(digit) / 2))
      .dropWhile(_ == '0')

    val result = leftHalf ++ center ++ leftHalf.reverse
    if result.isEmpty then "0" else result
