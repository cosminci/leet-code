package com.leetcode.cosminci._3000

object _2938_SeparateBlackAndWhiteBalls:

  def minimumSteps(s: String): Long =
    s.foldLeft(0L, 0) {
      case ((res, cnt), '1') => (res, cnt + 1)
      case ((res, cnt), _)   => (res + cnt, cnt)
    }._1
