package com.leetcode.cosminci._1100

import scala.util.chaining.*

object _1011_CapacityToShipPackagesWithinDDays:

  def shipWithinDays(weights: Array[Int], maxDays: Int): Int =
    def canShip(capacity: Int): Boolean =
      weights
        .foldLeft((1, 0)) { case ((days, load), w) => if load + w > capacity then (days + 1, w) else (days, load + w) }
        .pipe { case (days, _) => days <= maxDays }

    @annotation.tailrec
    def search(l: Int, r: Int): Int =
      if l >= r then l
      else
        val mid = l + (r - l) / 2
        if canShip(mid) then search(l, mid) else search(mid + 1, r)

    search(l = weights.max, r = weights.sum)
