package com.leetcode.cosminci._1600

import scala.collection.immutable.SortedSet

object _1514_PathWithMaxProfitability:

  def maxProbability(n: Int, edges: Array[Array[Int]], succProb: Array[Double], start: Int, end: Int): Double =
    val graph = edges.zip(succProb).foldLeft(Map.empty[Int, Seq[(Int, Double)]].withDefaultValue(Seq.empty)) {
      case (acc, (Array(a, b), prob)) =>
        acc.updated(a, acc(a) :+ (b -> prob)).updated(b, acc(b) :+ (a -> prob))
    }
    @annotation.tailrec
    def dfs(toVisit: SortedSet[(Int, Double)], visited: Map[Int, Double]): Double =
      toVisit.headOption match
        case None                              => 0.0
        case Some((curr, prob)) if curr == end => prob
        case Some((curr, prob)) =>
          val (nextToVisit, nextVisited) = graph(curr).foldLeft((toVisit.tail, visited)) {
            case ((toVisit, visited), (nei, neiProb)) =>
              visited.get(nei) match
                case Some(prevProb) if prevProb >= prob * neiProb => (toVisit, visited)
                case _ => (toVisit + (nei -> prob * neiProb), visited + (nei -> prob * neiProb))
          }
          dfs(nextToVisit, nextVisited)

    dfs(toVisit = SortedSet(start -> 1.0)(Ordering.by { case (n, p) => (-p, n) }), visited = Map(start -> 1.0))
