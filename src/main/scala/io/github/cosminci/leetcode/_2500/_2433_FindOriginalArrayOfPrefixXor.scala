package io.github.cosminci.leetcode._2500

object _2433_FindOriginalArrayOfPrefixXor:

  def findArray(pref: Array[Int]): Array[Int] =
    (0 +: pref).sliding(2).map(_.reduce(_ ^ _)).toArray
