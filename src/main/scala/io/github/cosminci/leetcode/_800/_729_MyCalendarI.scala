package io.github.cosminci.leetcode._800

import java.util.{Comparator, TreeMap}
import scala.util.Try

object _729_MyCalendarI:
  def main(args: Array[String]): Unit =
    val calendar = new MyCalendar()
    println(calendar.book(10, 20))
    println(calendar.book(15, 25))
    println(calendar.book(20, 30))

  class MyCalendar():
    private val bookings = new TreeMap[Int, Int]

    def book(start: Int, end: Int): Boolean =
      val prevEnd   = Try(bookings.floorEntry(start).getValue).getOrElse(Int.MinValue)
      val nextStart = Try(bookings.ceilingEntry(start).getKey).getOrElse(Int.MaxValue)
      if prevEnd > start || nextStart < end then return false

      bookings.put(start, end)
      true
