package com.leetcode.cosminci._900

import com.leetcode.cosminci.utils.gcd

object _878_NthMagicalNumber:
  def nthMagicalNumber(n: Int, a: Int, b: Int): Int =
    val lcm = a * b / gcd(a, b)
    @annotation.tailrec
    def binarySearch(l: Long, r: Long): Int =
      if l >= r then (l % 1_000_000_007).toInt
      else
        val mid = l + (r - l) / 2
        if mid / a + mid / b - mid / lcm < n then binarySearch(mid + 1, r)
        else binarySearch(l, mid)
    binarySearch(l = 2L, r = n.toLong * a.min(b))
