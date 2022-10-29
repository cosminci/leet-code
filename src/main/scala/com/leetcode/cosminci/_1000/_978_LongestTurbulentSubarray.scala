package com.leetcode.cosminci._1000

object _978_LongestTurbulentSubarray:
  def main(args: Array[String]): Unit =
    println(maxTurbulenceSize(Array(9, 4, 2, 10, 7, 8, 8, 1, 9)))
    println(maxTurbulenceSize(Array(4, 8, 12, 16)))
    println(maxTurbulenceSize(Array(100)))

  def maxTurbulenceSize(arr: Array[Int]): Int =
    (1 until arr.length)
      .foldLeft((1, 1, 1)) { case ((prevLengthWhenAsc, prevLengthWhenDesc, longest), i) =>
        val lengthWhenAsc  = if arr(i) > arr(i - 1) then prevLengthWhenDesc + 1 else 1
        val lengthWhenDesc = if arr(i) < arr(i - 1) then prevLengthWhenAsc + 1 else 1
        (lengthWhenAsc, lengthWhenDesc, math.max(longest, math.max(lengthWhenAsc, lengthWhenDesc)))
      }
      ._3
