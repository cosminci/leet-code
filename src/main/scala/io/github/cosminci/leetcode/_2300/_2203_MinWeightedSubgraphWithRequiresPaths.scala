package io.github.cosminci.leetcode._2300

import scala.collection.mutable

object _2203_MinWeightedSubgraphWithRequiresPaths:
  def main(args: Array[String]): Unit =
    println(
      minimumWeight(
        n = 6,
        edges = Array(
          Array(0, 2, 2),
          Array(0, 5, 6),
          Array(1, 0, 3),
          Array(1, 4, 5),
          Array(2, 1, 1),
          Array(2, 3, 3),
          Array(2, 3, 4),
          Array(3, 4, 2),
          Array(4, 5, 1)
        ),
        src1 = 0,
        src2 = 1,
        dest = 5
      )
    )

  def minimumWeight(n: Int, edges: Array[Array[Int]], src1: Int, src2: Int, dest: Int): Long =
    val emptyGraph = Map.empty[Int, Map[Int, Long]].withDefaultValue(Map.empty)
    val (graph1, graph2) = edges.foldLeft(emptyGraph, emptyGraph) { case ((g1, g2), Array(from, to, w)) =>
      (g1.updated(from, g1(from) + (to -> w)), g2.updated(to, g2(to) + (from -> w)))
    }

    def djikstra(src: Int, graph: Map[Int, Map[Int, Long]]): Array[Long] =
      val toVisit = mutable.PriorityQueue((0L, src))
      val visited = mutable.Map.empty[Int, Long]
      while toVisit.nonEmpty do
        val (cost, curr) = toVisit.dequeue()
        if !visited.contains(curr) then
          visited.update(curr, cost)
          graph(curr).foreach { case (next, newCost) =>
            toVisit.enqueue((cost + newCost, next))
          }
      Array.tabulate(n)(i => visited.getOrElse(i, Long.MaxValue))

    val costs1 = djikstra(src1, graph1)
    val costs2 = djikstra(src2, graph1)
    val costs3 = djikstra(dest, graph2)

    (0 until n)
      .flatMap { i =>
        if costs1(i) == Long.MaxValue || costs2(i) == Long.MaxValue || costs3(i) == Long.MaxValue then None
        else Some(costs1(i) + costs2(i) + costs3(i))
      }
      .minOption
      .getOrElse(-1)
