package com.leetcode.cosminci._2600

import scala.util.chaining.*

object _2566_MaxDifferenceByRemappingDigit:

  def minMaxDifference(num: Int): Int =
    val i   = num.toString.indexWhere(_ != '9')
    val min = if i == -1 then num.toString.head else num.toString.apply(i)
    val max = num.toString.head
    num.toString.replace(min, '9').toInt - num.toString.replace(max, '0').toInt
