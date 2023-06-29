package com.leetcode.cosminci._900

import scala.collection.immutable.BitSet
import scala.util.chaining.*

object _864_ShortestPathToGetAllKeys:

  def shortestPathAllKeys(grid: Array[String]): Int =
    val keyCount = grid.map(_.count(_.isLower)).sum
    val startX   = grid.indexWhere(_.contains('@'))
    val startY   = grid(startX).indexOf('@')

    Iterator
      .iterate((Set((startX, startY, BitSet.empty)), Set((startX, startY, BitSet.empty)), 0)) {
        case (toVisit, visited, steps) =>
          val nextToVisit = toVisit.flatMap { case (x, y, keys) =>
            if keys.size == keyCount then return steps
            Seq((x - 1, y), (x, y - 1), (x, y + 1), (x + 1, y))
              .filter { case (nx, ny) => nx >= 0 && nx < grid.length && ny >= 0 && ny < grid.head.length }
              .filter { case (nx, ny) => !visited.contains((nx, ny, keys)) }
              .filter { case (nx, ny) => grid(nx)(ny) != '#' }
              .filter { case (nx, ny) => !grid(nx)(ny).isUpper || keys.contains(grid(nx)(ny).asDigit) }
              .map { case (nx, ny) => (nx, ny, if grid(nx)(ny).isLower then keys.incl(grid(nx)(ny).asDigit) else keys) }
          }
          (nextToVisit, visited | nextToVisit, steps + 1)
      }
      .dropWhile { case (toVisit, _, _) => toVisit.nonEmpty }.next()
      .pipe(_ => -1)
