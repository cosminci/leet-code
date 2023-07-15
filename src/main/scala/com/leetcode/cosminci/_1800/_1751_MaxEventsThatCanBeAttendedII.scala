package com.leetcode.cosminci._1800

import scala.collection.mutable
import scala.util.chaining.*

object _1751_MaxEventsThatCanBeAttendedII:

  def maxValue(events: Array[Array[Int]], k: Int): Int =
    implicit val evOrd: Ordering[Array[Int]] = Ordering.by(arr => (arr(0), arr(1), arr(2)))
    events.sortInPlaceBy { case Array(s, e, v) => (s, e, v) }

    @annotation.tailrec
    def search(l: Int, r: Int, target: Int): Int =
      if l >= r then l
      else
        val mid = l + (r - l) / 2
        if events(mid)(1) <= target then search(mid + 1, r, target)
        else search(l, mid, target)

    val mem = mutable.Map.empty[(Int, Int), Int]
    def dfs(i: Int, k: Int): Int = mem.getOrElseUpdate((i, k),
      if i == events.length || k == 0 then 0
      else events
        .search(Array(events(i)(1), Int.MaxValue, Int.MaxValue), from = i + 1, to = events.length)
        .insertionPoint
        .pipe(j => dfs(i + 1, k).max(events(i)(2) + dfs(j, k - 1)))
    )

    dfs(i = 0, k)
