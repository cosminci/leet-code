package io.github.cosminci.leetcode._2000

import scala.collection.mutable

object _1970_LastDayWhereYouCanStillCross:
  def main(args: Array[String]): Unit =
    println(latestDayToCross(3, 3, Array(Array(1, 2), Array(2, 1), Array(3, 3), Array(2, 2), Array(1, 1))))

  private def latestDayToCross(numRows: Int, numCols: Int, cells: Array[Array[Int]]): Int =
    val firstRowCells = (1 to numCols).map(c => (1, c))
    val floodedCellsPerDay = cells.scanLeft(Set.empty[(Int, Int)]) { case (alreadyFlooded, Array(x, y)) =>
      alreadyFlooded + ((x, y))
    }

    def neighbours(r: Int, c: Int): Seq[(Int, Int)] =
      val below = Option.when(r < numRows)((r + 1, c))
      val left  = Option.when(c > 1)((r, c - 1))
      val right = Option.when(c < numCols)((r, c + 1))
      val above = Option.when(r > 1)((r - 1, c))
      Seq(above, below, left, right).flatten

    def canReach(day: Int): Boolean =
      val floodedCells = floodedCellsPerDay(day)
      val startCells   = firstRowCells.filter(cell => !floodedCells.contains(cell))
      val toVisit      = mutable.Stack.from(startCells)
      val visited      = mutable.Set.from(startCells)

      while toVisit.nonEmpty do
        val cell @ (r, c) = toVisit.pop()
        if r == numRows then return true

        neighbours(r, c).foreach { neighbour =>
          if !visited.contains(neighbour) && !floodedCells.contains(neighbour) then
            visited.add(neighbour)
            toVisit.push(neighbour)
        }

      false

    def binarySearch(left: Int, right: Int): Int =
      var (l, r) = (left, right)
      while l < r do
        val mid = l + (r - l) / 2
        if canReach(mid) then l = mid + 1
        else r = mid
      l - 1

    binarySearch(1, cells.length)
