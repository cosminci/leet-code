package com.leetcode.cosminci._100

import scala.util.chaining.*

object _7_ReverseInteger:

  def reverse(n: Int): Int =
    if n == Int.MinValue then 0
    else Iterator
      .iterate((n, 0)) { case (x, res) => (x / 10, res * 10 + x % 10) }
      .dropWhile { case (x, _) => x.abs > 9 }.next()
      .pipe { case (x, res) =>
        if res.abs > Int.MaxValue / 10 then 0
        else if res.abs == Int.MaxValue / 10 && (x > 7 || x < -8) then 0
        else res * 10 + x
      }
