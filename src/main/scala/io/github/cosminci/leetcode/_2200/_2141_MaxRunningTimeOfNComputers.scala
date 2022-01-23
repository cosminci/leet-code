package io.github.cosminci.leetcode._2200

object _2141_MaxRunningTimeOfNComputers:
  def maxRunTime(n: Int, batteries: Array[Int]): Long =
    def canRunComputersFor(time: Long) =
      batteries.map(_.toLong.min(time)).sum >= n * time

    @annotation.tailrec
    def binarySearch(l: Long, r: Long): Long =
      if l >= r then l
      else
        val mid = (l + r + 1) / 2
        if canRunComputersFor(mid) then binarySearch(mid, r)
        else binarySearch(l, mid - 1)

    binarySearch(l = 0L, r = batteries.map(_.toLong).sum / n)
