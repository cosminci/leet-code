package com.leetcode.cosminci._1100

object _1011_CapacityToShipPackagesWithinDDays:

  def main(args: Array[String]): Unit =
    println(shipWithinDays(Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 5))
    println(shipWithinDays(Array(3, 2, 2, 4, 1, 4), 3))
    println(shipWithinDays(Array(1, 2, 3, 1, 1), 4))

  def shipWithinDays(weights: Array[Int], days: Int): Int =
    def canShip(capacity: Int): Boolean =
      weights
        .foldLeft((1, 0)) { case ((days, load), w) =>
          if load + w > capacity then (days + 1, w)
          else (days, load + w)
        }
        ._1 <= days

    var (l, r) = (weights.max, weights.sum)
    while l < r do
      val mid = l + (r - l) / 2
      if canShip(mid) then r = mid
      else l = mid + 1
    l
