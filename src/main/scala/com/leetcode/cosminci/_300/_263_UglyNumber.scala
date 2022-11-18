package com.leetcode.cosminci._300

object _263_UglyNumber:

  def isUgly(n: Int): Boolean =
    Seq(2, 3, 5).foldLeft(n) { (n, divisor) =>
      Iterator
        .iterate(n)(_ / divisor)
        .dropWhile(n => n != 0 && n % divisor == 0)
        .next()
    } == 1
