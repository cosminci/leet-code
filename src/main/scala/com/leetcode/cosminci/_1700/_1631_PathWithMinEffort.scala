package com.leetcode.cosminci._1700

import scala.collection.mutable

import com.leetcode.cosminci.utils

object _1631_PathWithMinEffort:

  def minimumEffortPath(heights: Array[Array[Int]]): Int =
    val (m, n)    = (heights.length, heights.head.length)
    val toVisit   = mutable.PriorityQueue.from(Seq((0, 0, 0)))
    val minEffort = Array.tabulate(m, n) { case (x, y) => if x == 0 && y == 0 then 0 else Int.MaxValue }

    @annotation.tailrec
    def dfs(): Int =
      val (effort, x, y) = toVisit.dequeue()
      if x == m - 1 && y == n - 1 then return -effort
      utils
        .neighbours(x, y, heights)
        .foreach { case (nx, ny) =>
          val newEffort = (heights(nx)(ny) - heights(x)(y)).abs.max(-effort)
          if newEffort < minEffort(nx)(ny) then
            minEffort(nx)(ny) = newEffort
            toVisit.enqueue((-newEffort, nx, ny))
        }
      dfs()

    dfs()
