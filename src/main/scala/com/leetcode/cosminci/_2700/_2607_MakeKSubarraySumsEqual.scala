package com.leetcode.cosminci._2700

import com.leetcode.cosminci.utils.gcd

object _2607_MakeKSubarraySumsEqual:

  def makeSubKSumEqual(arr: Array[Int], k: Int): Long =
    val groupSize = gcd(k, arr.length)
    (0 until groupSize).foldLeft(0L) { (res, i) =>
      val group  = (i until arr.length by groupSize).map(arr.apply).sorted
      val median = group(group.length / 2)
      res + group.map(v => (median - v).abs.toLong).sum
    }
