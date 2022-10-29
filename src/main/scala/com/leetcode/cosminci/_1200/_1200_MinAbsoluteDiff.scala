package com.leetcode.cosminci._1200

object _1200_MinAbsoluteDiff:
  def minimumAbsDifference(arr: Array[Int]): List[List[Int]] =
    arr.sortInPlace()
    val min = arr.zip(arr.tail).map { case (a, b) => b - a }.min
    arr.zip(arr.tail).collect { case (a, b) if b - a == min => List(a, b) }.toList
