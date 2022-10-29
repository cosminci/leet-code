package com.leetcode.cosminci._2300

import scala.collection.mutable

object _2212_MaxPointsInArcheryCompetition:
  def maximumBobPoints(numArrows: Int, aliceArrows: Array[Int]): Array[Int] =
    val mem = mutable.Map.empty[(Int, Int), Int]
    def dfs(i: Int, arrowsLeft: Int): Int = mem.getOrElseUpdate((i, arrowsLeft), {
      if i == aliceArrows.length || arrowsLeft == 0 then 0
      else
        dfs(i + 1, arrowsLeft).max {
          if arrowsLeft > aliceArrows(i) then i + dfs(i + 1, arrowsLeft - aliceArrows(i) - 1)
          else 0
        }
    })

    val (bobArrows, arrowsLeft) = aliceArrows.indices.foldLeft(Array.empty[Int], numArrows) {
      case ((bobArrows, arrowsLeft), i) =>
        if dfs(i, arrowsLeft) == dfs(i + 1, arrowsLeft) then (bobArrows :+ 0, arrowsLeft)
        else (bobArrows :+ aliceArrows(i) + 1, arrowsLeft - aliceArrows(i) - 1)
    }

    bobArrows.updated(0, bobArrows(0) + arrowsLeft)
