package com.leetcode.cosminci._400

import scala.util.chaining.*
import scala.Integral.Implicits.*

object _400_NthDigit:

  def findNthDigit(n: Int): Int =
    Iterator
      .iterate((n - 1L, 1L, 1L)) { case (n, base, len) => (n - 9 * base * len, base * 10, len + 1) }
      .dropWhile { case (n, base, len) => n > 9 * base * len }.next()
      .pipe { case (n, base, len) => (base + n / len).toString.charAt((n % len).toInt) - '0' }

