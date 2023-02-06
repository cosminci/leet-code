package com.leetcode.cosminci._2600

import scala.util.chaining.*

object _2555_MaximizeWinFromTwoSegments:

  def maximizeWin(prizes: Array[Int], k: Int): Int =
    prizes.indices
      .foldLeft(Array(0), 0, 0) { case ((dp, j, res), i) =>
        Iterator
          .iterate(j)(_ + 1)
          .dropWhile(j => prizes(j) < prizes(i) - k)
          .next()
          .pipe(j => (dp :+ dp(i).max(i - j + 1), j, res.max(i - j + 1 + dp(j))))
      }._3
