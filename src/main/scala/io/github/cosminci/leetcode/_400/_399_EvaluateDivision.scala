package io.github.cosminci.leetcode._400

import scala.collection.mutable

object _399_EvaluateDivision:
  def main(args: Array[String]): Unit =
    println(
      calcEquationBFS(
        List(List("a", "b"), List("b", "c")),
        Array(2.0, 3.0),
        List(List("a", "c"), List("b", "a"), List("a", "e"), List("a", "a"), List("x", "x"))
      ).toSeq
    )

  def calcEquationBFS(
      equations: List[List[String]],
      values: Array[Double],
      queries: List[List[String]]
  ): Array[Double] =
    val graph =
      equations.zip(values).foldLeft(Map.empty[String, Seq[(String, Double)]]) {
        case (adj, (List(x, y), result)) =>
          adj.updatedWith(x) {
            case None             => Some(Seq((y, result)))
            case Some(neighbours) => Some(neighbours :+ (y, result))
          }.updatedWith(y) {
            case None             => Some(Seq((x, 1 / result)))
            case Some(neighbours) => Some(neighbours :+ (x, 1 / result))
          }
      }

    def bfs(x: String, y: String): Double =
      val toVisit = mutable.Queue((x, 1.0))
      val visited = mutable.Set(x
      )
      while toVisit.nonEmpty do
        val (curr, value) = toVisit.dequeue()
        if curr == y then return value

        graph(curr).foreach { case (neighbour, cost) =>
          if !visited.contains(neighbour) then
            toVisit.enqueue((neighbour, value * cost))
            visited.add(neighbour)
        }
      -1.0

    queries.map { case List(x, y) =>
      if !graph.contains(x) || !graph.contains(y) then -1.0
      else bfs(x, y)
    }.toArray
