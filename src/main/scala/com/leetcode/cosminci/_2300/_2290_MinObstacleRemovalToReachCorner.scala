package com.leetcode.cosminci._2300

import com.leetcode.cosminci.utils

import scala.collection.mutable

object _2290_MinObstacleRemovalToReachCorner:

  def minimumObstacles(grid: Array[Array[Int]]): Int =
    val (m, n)  = (grid.length - 1, grid.head.length - 1)
    val toVisit = mutable.PriorityQueue((0, 0, 0))
    val visited = mutable.Map((0, 0) -> 0)

    while toVisit.nonEmpty do
      val (steps, x, y) = toVisit.dequeue()
      if x == m && y == n then return -steps

      utils.neighbours(x, y, grid).foreach { case (nx, ny) =>
        val nsteps = steps - grid(nx)(ny)
        if nsteps > visited.getOrElse((nx, ny), Int.MinValue) then
          toVisit.enqueue((nsteps, nx, ny))
          visited.update((nx, ny), nsteps)
      }

    Int.MaxValue
