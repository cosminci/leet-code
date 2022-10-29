package com.leetcode.cosminci._1100

import scala.collection.mutable

object _1091_ShortestPathInBinaryMatrix:

  def shortestPathBinaryMatrix(grid: Array[Array[Int]]): Int =
    def neighbours(m: Int, n: Int, x: Int, y: Int): Seq[(Int, Int)] =
      Seq((-1, 0), (0, -1), (0, 1), (1, 0), (-1, -1), (-1, 1), (1, -1), (1, 1))
        .collect { case (dx, dy) if x + dx >= 0 && x + dx <= m && y + dy >= 0 && y + dy <= n => (x + dx, y + dy) }

    val (m, n)  = (grid.length - 1, grid(0).length - 1)
    val toVisit = mutable.Queue.empty[(Int, Int, Int)]
    val visited = mutable.Set((0, 0))

    if grid(0)(0) == 0 then toVisit.enqueue((0, 0, 0))

    while toVisit.nonEmpty do
      val (d, x, y) = toVisit.dequeue()
      if x == m && y == n then return d + 1
      else
        neighbours(m, n, x, y).foreach { case next @ (nx, ny) =>
          if !visited.contains(next) && grid(nx)(ny) == 0 then
            visited.add(next)
            toVisit.enqueue((d + 1, nx, ny))
        }

    -1
