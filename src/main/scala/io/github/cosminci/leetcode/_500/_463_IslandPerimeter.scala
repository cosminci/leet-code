package io.github.cosminci.leetcode._500

object _463_IslandPerimeter:

  def islandPerimeter(grid: Array[Array[Int]]): Int =
    def isWater(x: Int, y: Int): Boolean = x < 0 || x == grid.length || y < 0 || y == grid(x).length || grid(x)(y) == 1

    var perimeter = 0
    grid.indices.foreach { x =>
      grid(x).indices.foreach { y =>
        if grid(x)(y) == 0 then
          perimeter += (if isWater(x - 1, y) then 1 else 0)
          perimeter += (if isWater(x + 1, y) then 1 else 0)
          perimeter += (if isWater(x, y + 1) then 1 else 0)
          perimeter += (if isWater(x, y - 1) then 1 else 0)
      }
    }
    perimeter
