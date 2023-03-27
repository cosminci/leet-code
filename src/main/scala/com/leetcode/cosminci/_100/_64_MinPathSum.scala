package com.leetcode.cosminci._100

import scala.collection.immutable.TreeSet
import scala.util.chaining.*

object _64_MinPathSum:

  def minPathSum(grid: Array[Array[Int]]): Int =
    val toVisit = TreeSet((0, 0, grid(0)(0)))(Ordering.by { case (x, y, cost) => (cost, x, y) })
    val visited = Set((0, 0))
    val (m, n)  = (grid.length - 1, grid.head.length - 1)

    Iterator
      .iterate((toVisit, visited)) { case (toVisit, visited) =>
        val (x, y, pathCost) = toVisit.head
        Seq((x + 1, y), (x, y + 1))
          .filter { case (nx, ny) => nx <= m && ny <= n && !visited.contains((nx, ny)) }
          .foldLeft(toVisit.tail, visited) { case ((toVisit, visited), (nx, ny)) =>
            (toVisit + ((nx, ny, pathCost + grid(nx)(ny))), visited + ((nx, ny)))
          }
      }
      .dropWhile { case (toVisit, _) => toVisit.headOption.exists { case (x, y, _) => x != m || y != n } }.next()
      .pipe { case (toVisit, _) => toVisit.head.pipe { case (_, _, cost) => cost } }
