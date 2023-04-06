package com.leetcode.cosminci._1300

import scala.util.chaining.*

object _1254_NumClosedIslands:

  def closedIsland(grid: Array[Array[Int]]): Int =
    def explore(i: Int, j: Int, visited: Set[(Int, Int)]): (Boolean, Set[(Int, Int)]) =
      if i < 0 || j < 0 || i >= grid.length || j >= grid(i).length then (false, visited)
      else if grid(i)(j) == 1 then (true, visited)
      else Seq((i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1))
        .filterNot(visited.contains)
        .foldLeft((true, visited)) { case ((isClosed, visited), (i, j)) =>
          explore(i, j, visited + ((i, j))).pipe { case (isNeiClosed, visited) => (isClosed && isNeiClosed, visited) }
        }

    grid.indices.foldLeft(0, Set.empty[(Int, Int)]) { case ((count, visited), i) =>
      grid(i).indices.foldLeft(count, visited) { case ((count, visited), j) =>
        if grid(i)(j) == 1 || visited.contains((i, j)) then (count, visited)
        else explore(i, j, visited + ((i, j))).pipe { case (c, v) => (count + (if c then 1 else 0), v) }
      }
    }.pipe { case (count, _) => count }
