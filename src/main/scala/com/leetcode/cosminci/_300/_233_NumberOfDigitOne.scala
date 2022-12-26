package com.leetcode.cosminci._300

import scala.util.chaining.*

object _233_NumberOfDigitOne:

  def countDigitOne(n: Int): Int =
    Iterator
      .iterate((0, 1)) { case (res, i) => (res + (n / (i * 10)) * i + (n % (i * 10) - i + 1).max(0).min(i), i * 10) }
      .dropWhile { case (_, i) => i <= n }.next()
      .pipe { case (res, i) => res }
