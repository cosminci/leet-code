package com.leetcode.cosminci._3100

object _3074_AppleRedistributionIntoBoxes:

  def minimumBoxes(apple: Array[Int], capacity: Array[Int]): Int =
    capacity.sorted.reverse.scanLeft(0)(_ + _).indexWhere(_ >= apple.sum)
