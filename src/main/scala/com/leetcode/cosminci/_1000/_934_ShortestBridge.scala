package com.leetcode.cosminci._1000

import scala.collection.mutable

object _934_ShortestBridge:
  def main(args: Array[String]): Unit =
    println(
      shortestBridge(
        Array(
          Array(0, 0, 1, 0, 1),
          Array(0, 1, 1, 0, 1),
          Array(0, 1, 0, 0, 1),
          Array(0, 0, 0, 0, 0),
          Array(0, 0, 0, 0, 0)
        )
      )
    )

  def shortestBridge(grid: Array[Array[Int]]): Int =
    def neighbours(x: Int, y: Int) =
      Seq((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)).filter { case (x, y) =>
        grid.isDefinedAt(x) && grid(x).isDefinedAt(y)
      }

    def markIsland(x: Int, y: Int): Seq[(Int, Int)] =
      val island = mutable.Set((x, y))
      def dfs(x: Int, y: Int): Unit =
        grid(x)(y) = 2
        neighbours(x, y).foreach { case (x, y) =>
          if grid(x)(y) == 1 then
            island.add((x, y))
            dfs(x, y)
        }
      dfs(x, y)
      island.toSeq

    val Some((x, y)) = grid.indices.collectFirst {
      case x if grid(x).contains(1) =>
        (x, grid(x).indexWhere(_ == 1))
    }

    val island  = markIsland(x, y)
    val toVisit = mutable.Queue.from(island.map { case (x, y) => (x, y, 0) })
    val visited = mutable.Set.from(island)
    while toVisit.nonEmpty do
      val (x, y, d) = toVisit.dequeue()
      if grid(x)(y) == 1 then return d - 1
      neighbours(x, y).foreach { case (x, y) =>
        if grid(x)(y) != 2 && !visited.contains((x, y)) then
          visited.add((x, y))
          toVisit.enqueue((x, y, d + 1))
      }
    0
