package io.github.cosminci.leetcode._400

import scala.collection.mutable

object _399_EvaluateDivision:
  def main(args: Array[String]): Unit =
    println(
      calcEquation(
        List(List("a", "b"), List("b", "c")),
        Array(2.0, 3.0),
        List(List("a", "c"), List("b", "a"), List("a", "e"), List("a", "a"), List("x", "x"))
      ).toSeq
    )

  def calcEquation(
      equations: List[List[String]],
      values: Array[Double],
      queries: List[List[String]]
  ): Array[Double] =
    val graph = buildGraph(equations, values)

    queries.map { eq =>
      if !graph.contains(eq.head) || !graph.contains(eq.last) then -1.0
      else bfs(graph, eq.head, eq.last)
    }.toArray

  private def buildGraph(equations: List[List[String]], values: Array[Double]) =
    equations.zip(values).foldLeft(Map.empty[String, Seq[(String, Double)]]) {
      case (adj, (eq, result)) =>
        adj.updatedWith(eq.head) {
          case None             => Some(Seq((eq.last, result)))
          case Some(neighbours) => Some(neighbours :+ (eq.last, result))
        }.updatedWith(eq.last) {
          case None             => Some(Seq((eq.head, 1 / result)))
          case Some(neighbours) => Some(neighbours :+ (eq.head, 1 / result))
        }
    }

  private def bfs(graph: Map[String, Seq[(String, Double)]], x: String, y: String): Double =
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
