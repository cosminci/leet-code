package com.leetcode.cosminci._400

object _365_WaterAndJugProblem:

  def canMeasureWater(jug1Capacity: Int, jug2Capacity: Int, targetCapacity: Int): Boolean =
    Iterator
      .iterate((Seq((0, 0)), Set((0, 0)))) { case ((jug1, jug2) +: toVisit, visited) =>
        val nextUnvisited = Seq(
          (jug1Capacity, jug2),
          (jug1, jug2Capacity),
          (0, jug2),
          (jug1, 0),
          (jug1Capacity.min(jug1 + jug2), 0.max(jug2 + jug1 - jug1Capacity)),
          (0.max(jug1 + jug2 - jug2Capacity), jug2Capacity.min(jug1 + jug2))
        ).filterNot(visited.contains)

        (toVisit ++ nextUnvisited, visited ++ nextUnvisited)
      }
      .takeWhile { case (toVisit, _) => toVisit.nonEmpty }
      .exists { case ((jug1, jug2) +: toVisit, _) => jug1 + jug2 == targetCapacity }
