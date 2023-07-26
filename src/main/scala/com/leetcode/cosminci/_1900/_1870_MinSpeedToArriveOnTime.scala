package com.leetcode.cosminci._1900

import scala.util.chaining.*

object _1870_MinSpeedToArriveOnTime:

  def minSpeedOnTime(dist: Array[Int], hour: Double): Int =
    def canReach(speed: Int) =
      dist.foldLeft(0.0)((time, d) => time.ceil + (d.toDouble / speed)) <= hour

    @annotation.tailrec
    def search(l: Int, r: Int): Int =
      if l > r then l
      else
        val mid = l + (r - l) / 2
        if canReach(mid) then search(l, mid - 1)
        else search(mid + 1, r)

    if hour < dist.length - 1 then -1
    else search(l = 1, r = 10_000_000).pipe(s => if s > 10_000_000 then -1 else s)
