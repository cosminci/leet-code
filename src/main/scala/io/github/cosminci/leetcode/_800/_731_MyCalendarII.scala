package io.github.cosminci.leetcode._800

import scala.collection.mutable

object _731_MyCalendarII {
  def main(args: Array[String]): Unit = {
    val calendar = new MyCalendarTwo()
    println(calendar.book(24, 40))
    println(calendar.book(43, 50))
    println(calendar.book(27, 43))
    println(calendar.book(5, 21))
    println(calendar.book(30, 40))
    println(calendar.book(14, 29))
    println(calendar.book(3, 19))
    println(calendar.book(3, 14))
    println(calendar.book(25, 39))
    println(calendar.book(6, 19))
  }
  class MyCalendarTwo() {
    private val bookings = mutable.TreeMap.empty[Int, Int]
    def book(start: Int, end: Int): Boolean = {
      bookings.update(start, bookings.getOrElse(start, 0) + 1)
      bookings.update(end, bookings.getOrElse(end, 0) - 1)

      var overlapping = 0
      bookings.foreach {
        case (time, countDelta) =>
          overlapping += countDelta
          if (overlapping == 3) {
            bookings.update(start, bookings(start) - 1)
            bookings.update(end, bookings(end) + 1)
            return false
          }
      }
      true
    }
  }

}
