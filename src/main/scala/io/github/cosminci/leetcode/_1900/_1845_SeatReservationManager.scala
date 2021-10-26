package io.github.cosminci.leetcode._1900

import scala.collection.mutable

object _1845_SeatReservationManager:

  class SeatManager(n: Int):
    given Ordering[Int] = (x, y) => y.compareTo(x)

    private val freeSeats = mutable.PriorityQueue.from(1 to n)

    def reserve(): Int = freeSeats.dequeue()

    def unreserve(seatNumber: Int) = freeSeats.enqueue(seatNumber)
