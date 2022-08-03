package io.github.cosminci.leetcode._800

import java.util
import scala.util.Try

object _729_MyCalendarI:

  class MyCalendar:
    private val bookings = new util.TreeMap[Int, Int]

    def book(start: Int, end: Int): Boolean =
      val prevEnd   = Try(bookings.floorEntry(start).getValue).getOrElse(Int.MinValue)
      val nextStart = Try(bookings.ceilingEntry(start).getKey).getOrElse(Int.MaxValue)
      Option.when(prevEnd <= start && nextStart >= end)(bookings.put(start, end)).isDefined
