package io.github.cosminci.leetcode._2000

import io.github.cosminci.utils

import scala.collection.mutable

object _1905_CountSubIslands:

  def main(args: Array[String]): Unit =
    val grid1 = Array(
      Array(1, 1, 1, 0, 0),
      Array(0, 1, 1, 1, 1),
      Array(0, 0, 0, 0, 0),
      Array(1, 0, 0, 0, 0),
      Array(1, 1, 0, 1, 1)
    )
    val grid2 = Array(
      Array(1, 1, 1, 0, 0),
      Array(0, 0, 1, 1, 1),
      Array(0, 1, 0, 0, 0),
      Array(1, 0, 1, 1, 0),
      Array(0, 1, 0, 1, 0)
    )
    println(countSubIslands(grid1, grid2))

  private def countSubIslands(grid1: Array[Array[Int]], grid2: Array[Array[Int]]): Int =
    var count        = 0
    val grid2Visited = mutable.Set.empty[(Int, Int)]
    grid2.indices.foreach { x =>
      grid2(x).indices.foreach { y =>
        if grid2(x)(y) == 1 && !grid2Visited.contains((x, y)) then
          val toVisit = mutable.Queue.empty[(Int, Int)]
          grid2Visited.add((x, y))
          toVisit.enqueue((x, y))
          var matched = grid1(x)(y) == 1
          while toVisit.nonEmpty do
            val (coordX, coordY) = toVisit.dequeue()
            utils.neighbours(coordX, coordY, grid2).foreach { case n @ (nx, ny) =>
              if !grid2Visited.contains(n) then
                toVisit.enqueue(n)
                grid2Visited.add((nx, ny))
                if grid1(nx)(ny) == 0 then matched = false
            }
          if matched then count += 1
      }
    }
    count
