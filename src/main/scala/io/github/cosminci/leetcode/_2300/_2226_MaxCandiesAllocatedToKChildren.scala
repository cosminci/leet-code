package io.github.cosminci.leetcode._2300

object _2226_MaxCandiesAllocatedToKChildren:
  def maximumCandies(candies: Array[Int], k: Long): Int =
    def canAllocate(count: Int): Boolean =
      candies.foldLeft(0L)((kidsHappy, pile) => kidsHappy + pile / count) >= k

    @annotation.tailrec
    def binarySearch(l: Int, r: Int): Int =
      if l >= r then l
      else
        val mid = (l + r + 1) / 2
        if canAllocate(mid) then binarySearch(l = mid, r)
        else binarySearch(l, mid - 1)

    binarySearch(l = 0, r = candies.max)
