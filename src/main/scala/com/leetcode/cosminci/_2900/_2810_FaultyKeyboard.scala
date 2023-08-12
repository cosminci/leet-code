package com.leetcode.cosminci._2900

import scala.util.chaining.*

object _2810_FaultyKeyboard:

  def finalString(s: String): String =
    s.foldLeft("", false) { case ((res, flip), ch) =>
      if ch == 'i' then (res, !flip)
      else if flip then (ch +: res, flip)
      else (res :+ ch, flip)
    }.pipe { case (res, flip) => if flip then res.reverse else res }
