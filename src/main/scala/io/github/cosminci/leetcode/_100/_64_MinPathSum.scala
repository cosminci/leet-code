package io.github.cosminci.leetcode._100

import scala.collection.mutable

object _64_MinPathSum:
  def main(args: Array[String]): Unit =
    println(minPathSumDjikstra(Array(Array(1))))
    println(minPathSumDP(Array(Array(1, 2, 3), Array(4, 5, 6))))

  private def minPathSumDP(grid: Array[Array[Int]]): Int =
    val dp = Array.ofDim[Int](grid.head.length)
    dp.indices.foreach { i =>
      dp(i) = grid(0)(i) + (if i > 0 then dp(i - 1) else 0)
    }

    grid.indices.tail.foreach { row =>
      var fromLeftCost = Int.MaxValue
      grid(row).indices.foreach { col =>
        val fromAboveCost = dp(col)
        dp(col) = grid(row)(col) + math.min(fromAboveCost, fromLeftCost)
        fromLeftCost = dp(col)
      }
    }

    dp.last

  private def minPathSumDjikstra(grid: Array[Array[Int]]): Int =
    given Ordering[(Int, Int, Int)] = (p1, p2) => p2._3.compareTo(p1._3)

    val toVisit = mutable.PriorityQueue((0, 0, grid(0)(0)))
    val visited = mutable.Set((0, 0))

    while toVisit.nonEmpty do
      val (x, y, pathCost) = toVisit.dequeue()
      if x == grid.length - 1 && y == grid.head.length - 1 then return pathCost
      reachable(grid, x, y).foreach { case reachable @ (rx, ry) =>
        if !visited.contains(reachable) then
          toVisit.enqueue((rx, ry, pathCost + grid(rx)(ry)))
          visited.add((rx, ry))
      }
    0 // never reached

  private def reachable(grid: Array[Array[Int]], x: Int, y: Int) =
    val lower = Option.when(x < grid.length - 1)((x + 1, y))
    val right = Option.when(y < grid.head.length - 1)((x, y + 1))
    List(lower, right).flatten
