package io.github.cosminci.leetcode._2200

object _2187_MinTimeToCompleteTrips:
  def minimumTime(time: Array[Int], totalTrips: Int): Long =
    @annotation.tailrec
    def binarySearch(l: Long, r: Long): Long =
      if l >= r then l
      else
        val mid = l + (r - l) / 2
        if time.map(mid / _).sum < totalTrips.toLong then binarySearch(mid + 1, r)
        else binarySearch(l, mid)

    binarySearch(1, totalTrips.toLong * time.max)
