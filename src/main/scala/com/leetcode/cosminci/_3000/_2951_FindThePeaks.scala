package com.leetcode.cosminci._3000

object _2951_FindThePeaks:

  def findPeaks(mountain: Array[Int]): List[Int] =
    mountain
      .sliding(3)
      .zipWithIndex
      .collect { case (Array(a, b, c), i) if b > a && b > c => i + 1 }
      .toList
