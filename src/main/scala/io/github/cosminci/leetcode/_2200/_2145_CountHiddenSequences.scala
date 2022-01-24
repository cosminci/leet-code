package io.github.cosminci.leetcode._2200

object _2145_CountHiddenSequences:

  def numberOfArrays(differences: Array[Int], lower: Int, upper: Int): Int =
    val balance    = differences.scanLeft(0L)(_ + _)
    val (min, max) = (balance.min, balance.max)
    ((upper - lower) - (max - min) + 1).max(0L).toInt
