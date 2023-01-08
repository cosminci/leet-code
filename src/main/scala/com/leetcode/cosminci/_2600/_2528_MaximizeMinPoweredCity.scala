package com.leetcode.cosminci._2600

import scala.util.chaining.*

object _2528_MaximizeMinPoweredCity:

  def maxPower(stations: Array[Int], r: Int, k: Int): Long =
    val n     = stations.length
    val power = (0 until n).zip(stations.map(_.toLong)).toMap
    val init  = stations.take(r).map(_.toLong).sum

    def canPower(targetPower: Long): Boolean =
      (0 until n)
        .foldLeft((init, power, 0L)) { case ((window, power, need), i) =>
          val powerToAdd    = if i + r < n then power(i + r) else 0
          val powerToRemove = if i - r > 0 then power(i - r - 1) else 0
          val newWindow     = window + powerToAdd - powerToRemove
          val missingPower  = (targetPower - newWindow).max(0)
          val newPower      = power.updated((n - 1).min(i + r), power((n - 1).min(i + r)) + missingPower)
          (newWindow + missingPower, newPower, need + missingPower)
        }
        .pipe { case (_, _, need) => need <= k }

    Iterator
      .iterate((0L, stations.map(_.toLong).sum + k)) { case (l, r) =>
        val mid = l + (r - l) / 2
        if canPower(mid) then (mid + 1, r)
        else (l, mid - 1)
      }
      .dropWhile { case (l, r) => l <= r }.next()
      .pipe { case (_, r) => r }
