package com.leetcode.cosminci._800

import scala.collection.mutable

object _778_SwimInRisingWater:

  def main(args: Array[String]): Unit =
    List(
      Array(
        Array(0, 1, 2, 3, 4),
        Array(24, 23, 22, 21, 5),
        Array(12, 13, 14, 15, 16),
        Array(11, 17, 18, 19, 20),
        Array(10, 9, 8, 7, 6)
      ),
      Array(Array(3, 2), Array(1, 0))
    ).foreach { grid =>
      println(swimInWaterDFSLoop(grid))
      println(swimInWaterDjikstra(grid))
    }

  def swimInWaterDjikstra(grid: Array[Array[Int]]): Int =
    given Ordering[Position] = (p1, p2) => p2.maxDepth.compare(p1.maxDepth)

    val toVisit = mutable.PriorityQueue(Position(0, 0, grid(0)(0)))
    val visited = mutable.Set((0, 0))
    while toVisit.nonEmpty do
      val Position(x, y, maxDepth) = toVisit.dequeue()
      if x == grid.length - 1 && y == grid(x).length - 1 then return maxDepth
      neighbours(x, y, grid).foreach { case n @ (nx, ny) =>
        if !visited.contains(n) then
          visited.add(n)
          toVisit.enqueue(Position(nx, ny, math.max(grid(nx)(ny), maxDepth)))
      }
    0

  def neighbours(x: Int, y: Int, grid: Array[Array[Int]]): Seq[(Int, Int)] =
    val above = Option.when(x > 0)((x - 1, y))
    val left  = Option.when(y > 0)((x, y - 1))
    val right = Option.when(y < grid(x).length - 1)((x, y + 1))
    val below = Option.when(x < grid.length - 1)((x + 1, y))
    List(above, left, right, below).flatten

  def swimInWaterDFSLoop(grid: Array[Array[Int]]): Int =
    val origins = mutable.Set((0, 0))
    var time    = grid(0)(0)
    while true do
      val toVisit = mutable.Stack.from(origins)
      while toVisit.nonEmpty do
        val (x, y) = toVisit.pop()
        reachable(x, y, time, grid).foreach { case n @ (nx, ny) =>
          if !origins.contains(n) then
            if nx == grid.length - 1 && ny == grid.length - 1 then return time
            toVisit.push(n)
            origins.add(n)
        }
      time += 1
    time

  def reachable(x: Int, y: Int, elevation: Int, grid: Array[Array[Int]]): Seq[(Int, Int)] =
    val above = Option.when(x > 0 && grid(x - 1)(y) <= elevation)((x - 1, y))
    val left  = Option.when(y > 0 && grid(x)(y - 1) <= elevation)((x, y - 1))
    val right = Option.when(y < grid(x).length - 1 && grid(x)(y + 1) <= elevation)((x, y + 1))
    val below = Option.when(x < grid.length - 1 && grid(x + 1)(y) <= elevation)((x + 1, y))
    List(above, left, right, below).flatten

  case class Position(x: Int, y: Int, maxDepth: Int)
