package io.github.cosminci.leetcode._500

import scala.collection.mutable

object _433_MinGeneticMutation:
  private def minMutation(start: String, end: String, bank: Array[String]): Int =
    def diffCount(s1: String, s2: String) =
      s1.zip(s2).count { case (c1, c2) => c1 != c2 } == 1

    val toVisit = mutable.Queue((start, 0))
    val visited = mutable.Set(start)

    while toVisit.nonEmpty do
      val (curr, steps) = toVisit.dequeue()
      if curr == end then return steps

      bank.foreach { next =>
        if !visited.contains(next) && diffCount(curr, next) then
          toVisit.enqueue((next, steps + 1))
          visited.add(next)
      }
    -1
