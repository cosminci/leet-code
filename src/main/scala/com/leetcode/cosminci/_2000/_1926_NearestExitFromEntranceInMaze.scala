package com.leetcode.cosminci._2000

import com.leetcode.cosminci.utils

import scala.collection.mutable

object _1926_NearestExitFromEntranceInMaze:

  def nearestExit(maze: Array[Array[Char]], entrance: Array[Int]): Int =
    val start   = (entrance.head, entrance.last)
    val toVisit = mutable.Queue((start, 0))
    val visited = mutable.Set(start)
    while toVisit.nonEmpty do
      val ((x, y), dist) = toVisit.dequeue()
      if (x == 0 || y == 0 || x == maze.length - 1 || y == maze.head.length - 1) && (x, y) != start then return dist
      utils
        .neighbours(x, y, maze)
        .filterNot { case n @ (nx, ny) => visited.contains(n) || maze(nx)(ny) == '+' }
        .foreach { n =>
          visited.add(n)
          toVisit.enqueue((n, dist + 1))
        }
    -1
