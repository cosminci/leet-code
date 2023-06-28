package com.leetcode.cosminci._2800

import com.leetcode.cosminci.utils.gcd

object _2748_NumBeautifulPairs:

  def countBeautifulPairs(nums: Array[Int]): Int =
    val head  = nums.map(v => Iterator.iterate(v)(_ / 10).dropWhile(_ >= 10).next())
    val last  = nums.map(_ % 10)
    val pairs = head.indices.flatMap(i => (i + 1 until last.length).map(j => (i, j)))
    pairs.count { case (i, j) => gcd(head(i), last(j)) == 1 }
