package io.github.cosminci.leetcode._100

object _34_FindFirstAndLastPositionOfElementInSortedArray:

  def searchRange(nums: Array[Int], target: Int): Array[Int] =
    def dfs(lo: Int, hi: Int): Seq[Int] =
      if nums(lo) == target && target == nums(hi) then Seq(lo, hi)
      else if target < nums(lo) || target > nums(hi) then Seq(-1, -1)
      else
        val mid    = lo + (hi - lo) / 2
        val (l, r) = (dfs(lo, mid), dfs(mid + 1, hi))
        if (l ++ r).contains(-1) then Seq(l, r).max else Seq(l.head, r.last)

    if nums.isEmpty then Array(-1, -1)
    else dfs(lo = 0, hi = nums.indices.last).toArray
