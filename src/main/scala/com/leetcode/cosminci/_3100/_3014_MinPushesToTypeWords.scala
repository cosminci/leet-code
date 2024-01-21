package com.leetcode.cosminci._3100

object _3014_MinPushesToTypeWords:

  def minimumPushes(word: String): Int =
    val cnt  = word.distinct.length
    val gt8  = (cnt - 8).max(0).min(8)
    val gt16 = (cnt - 16).max(0).min(8)
    val gt24 = (cnt - 24).max(0)
    cnt.min(8) + gt8 * 2 + gt16 * 3 + gt24 * 4
