package com.leetcode.cosminci._900

import com.leetcode.cosminci.utils

import scala.collection.mutable

object _827_MakingALargeIsland:
  def main(args: Array[String]): Unit =
    println(largestIsland(Array(Array(1, 1), Array(1, 1))))
    println(
      largestIsland(
        Array(
          Array(0, 0, 0, 0, 0, 0, 0),
          Array(0, 1, 1, 1, 1, 0, 0),
          Array(0, 1, 0, 0, 1, 0, 0),
          Array(1, 0, 1, 0, 1, 0, 0),
          Array(0, 1, 0, 0, 1, 0, 0),
          Array(0, 1, 0, 0, 1, 0, 0),
          Array(0, 1, 1, 1, 1, 0, 0)
        )
      )
    )

  def largestIsland(grid: Array[Array[Int]]): Int =
    var (islands, coordToIsland) = mapIslands(grid)
    if islands.size == 0 then return 1

    var maxSize = islands.values.maxBy(_.size).size
    if maxSize < grid.length * grid.head.length then maxSize += 1

    grid.indices.foreach { row =>
      grid(row).indices.foreach { col =>
        if grid(row)(col) == 0 then
          val adjacentCoords     = utils.neighbours(row, col, grid)
          val connectableIslands = adjacentCoords.flatMap(coordToIsland.get).distinct
          if connectableIslands.size > 0 then
            val connectableSize = connectableIslands.map(islands).map(_.size).sum + 1
            maxSize = math.max(maxSize, connectableSize)
      }
    }

    maxSize

  def mapIslands(grid: Array[Array[Int]]) =
    val islands       = mutable.Map.empty[Int, Vector[(Int, Int)]]
    val coordToIsland = mutable.Map.empty[(Int, Int), Int]

    var islandId = 0
    val visited  = mutable.Set.empty[(Int, Int)]
    grid.indices.foreach { row =>
      grid(row).indices.foreach { col =>
        if !visited.contains((row, col)) && grid(row)(col) == 1 then
          visited.add((row, col))
          val island  = mutable.ListBuffer.empty[(Int, Int)]
          val toVisit = mutable.Stack((row, col))
          while toVisit.nonEmpty do
            val coord @ (x, y) = toVisit.pop()
            island.append(coord)
            coordToIsland.update(coord, islandId)
            utils.neighbours(x, y, grid).foreach { case (nx, ny) =>
              if !visited.contains((nx, ny)) && grid(nx)(ny) == 1 then
                visited.add((nx, ny))
                toVisit.push((nx, ny))
            }
          islands.update(islandId, island.toVector)
          islandId += 1
      }
    }
    (islands.toMap, coordToIsland.toMap)
