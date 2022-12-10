package com.leetcode.cosminci._2500

import scala.util.Try

object _2496_MaxValueOfStringInArray:

  def maximumValue(strs: Array[String]): Int =
    strs.map(s => Try(s.toInt).getOrElse(s.length)).max
