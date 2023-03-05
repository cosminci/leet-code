package com.leetcode.cosminci._2600

import scala.util.chaining.*

object _2578_SplitWithMinSum:

  def splitNum(num: Int): Int =
    num.toString.sorted.zipWithIndex
      .foldLeft(0, 0) { case ((a, b), (ch, i)) =>
        if i % 2 == 0 then (a * 10 + ch - '0', b)
        else (a, b * 10 + ch - '0')
      }
      .pipe { case (a, b) => a + b }
