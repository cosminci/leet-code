package com.leetcode.cosminci._1800

object _1732_FindHighestAltitude:

  def largestAltitude(gain: Array[Int]): Int =
    gain.scanLeft(0)(_ + _).max
