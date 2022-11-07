package com.leetcode.cosminci._1400

import scala.util.chaining._

object _1323_Max69Num:

  def maximum69Number(num: Int): Int =
    num.toString.indexWhere(_ == '6').pipe { i =>
      if i == -1 then num
      else num.toString.updated(i, '9').toInt
    }
