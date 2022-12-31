package com.leetcode.cosminci._400

import scala.util.chaining.*

object _367_ValidPerfectSquare:

  def isPerfectSquare(num: Long): Boolean =
    Iterator
      .iterate((1L, num)) { case (l, r) => (l + (r - l) / 2).pipe(m => if m * m < num then (m + 1, r) else (l, m - 1)) }
      .takeWhile { case (l, r) => l <= r }
      .exists { case (l, r) => (l + (r - l) / 2).pipe(m => m * m == num) }
