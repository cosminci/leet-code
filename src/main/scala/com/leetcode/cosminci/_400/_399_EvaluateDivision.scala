package com.leetcode.cosminci._400

import scala.collection.mutable

object _399_EvaluateDivision:

  def calcEquation(
      equations: List[List[String]],
      values: Array[Double],
      queries: List[List[String]]
  ): Array[Double] = {
    val graph = equations.zip(values).foldLeft(Map.empty[String, Seq[(String, Double)]]) {
      case (adj, (eq, result)) =>
        adj.updated(eq.head, adj.getOrElse(eq.head, Seq.empty) :+ (eq.last, result))
          .updated(eq.last, adj.getOrElse(eq.last, Seq.empty) :+ (eq.head, 1 / result))
    }

    def bfs(x: String, y: String): Double =
      val toVisit = mutable.Queue((x, 1.0))
      val visited = mutable.Set(x)
      while toVisit.nonEmpty do
        val (curr, value) = toVisit.dequeue()
        if curr == y then return value
        graph(curr).foreach { case (neighbour, cost) =>
          if !visited.contains(neighbour) then
            toVisit.enqueue((neighbour, value * cost))
            visited.add(neighbour)
        }
      -1.0

    queries.map { eq =>
      if !graph.contains(eq.head) || !graph.contains(eq.last) then -1.0
      else bfs(eq.head, eq.last)
    }.toArray
  }


