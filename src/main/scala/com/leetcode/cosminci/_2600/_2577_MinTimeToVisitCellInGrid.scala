package com.leetcode.cosminci._2600

import scala.collection.mutable

object _2577_MinTimeToVisitCellInGrid:

  def minimumTime(grid: Array[Array[Int]]): Int =
    if grid(0)(1) > 1 && grid(1)(0) > 1 then return -1

    val (m, n)  = (grid.length, grid.head.length)
    val toVisit = mutable.PriorityQueue.from(Seq((0, 0, 0)))(Ordering.by { case (time, _, _) => -time })
    val visited = mutable.Set.empty[(Int, Int)]

    while toVisit.nonEmpty do
      val (time, x, y) = toVisit.dequeue()
      if x == m - 1 && y == n - 1 then return time
      Seq((x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y))
        .filter { case (x, y) => x >= 0 && x < m && y >= 0 && y < n }
        .filterNot(visited.contains)
        .foreach { case (nx, ny) =>
          val wait = if (grid(nx)(ny) - time) % 2 == 0 then 1 else 0
          toVisit.enqueue(((grid(nx)(ny) + wait).max(time + 1), nx, ny))
          visited.add((nx, ny))
        }
    -1
