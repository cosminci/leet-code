package io.github.cosminci.leetcode._1700

object _1642_FurthestBuildingYouCanReach:

  def furthestBuilding(heights: Array[Int], bricks: Int, ladders: Int): Int =
    def canReach(k: Int) =
      val toClimb = (1 to k).map(k => heights(k) - heights(k - 1)).filter(_ > 0)
      toClimb.length <= ladders || toClimb.sorted.dropRight(ladders).sum <= bricks

    @annotation.tailrec
    def search(l: Int, r: Int): Int =
      if l + 1 >= r then l
      else
        val mid = l + (r - l) / 2
        if canReach(mid) then search(l = mid, r)
        else search(l, r = mid)

    search(l = 0, r = heights.length)
