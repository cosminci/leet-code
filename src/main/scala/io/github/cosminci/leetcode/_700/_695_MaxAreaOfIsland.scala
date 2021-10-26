package io.github.cosminci.leetcode._700

import io.github.cosminci.utils

import scala.collection.mutable

object _695_MaxAreaOfIsland:

  def maxAreaOfIsland(grid: Array[Array[Int]]): Int =
    val visited = mutable.Set.empty[(Int, Int)]

    def islandSize(rootX: Int, rootY: Int): Int =
      visited.add((rootX, rootY))

      val toVisit = mutable.Queue((rootX, rootY))
      var size    = 0

      while toVisit.nonEmpty do
        val (currX, currY) = toVisit.dequeue()
        size += 1

        utils.neighbours(currX, currY, grid).foreach { case (nextX, nextY) =>
          if !visited.contains((nextX, nextY)) && grid(nextX)(nextY) == 1 then
            toVisit.enqueue((nextX, nextY))
            visited.add((nextX, nextY))
        }

      size

    val islandSizes = for
      x <- grid.indices
      y <- grid(x).indices
      if grid(x)(y) == 1 && !visited.contains((x, y))
    yield islandSize(x, y)

    islandSizes.maxOption.getOrElse(0)
