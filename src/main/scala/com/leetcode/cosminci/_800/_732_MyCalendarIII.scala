package com.leetcode.cosminci._800

import scala.collection.mutable

object _732_MyCalendarIII:

  class MyCalendarThree:
    private val balance = mutable.TreeMap.empty[Int, Int].withDefaultValue(0)

    def book(start: Int, end: Int): Int =
      balance.update(start, balance(start) + 1)
      balance.update(end, balance(end) - 1)
      balance
        .foldLeft(0, 0) { case ((max, curr), (_, v)) =>
          (max.max(curr + v), curr + v)
        }
        ._1
