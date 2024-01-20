package com.leetcode.cosminci._3000

import scala.util.chaining.*

object _2976_MinCostToConvertStringI:

  def minimumCost(source: String, target: String, original: Array[Char], changed: Array[Char], cost: Array[Int]): Long =
    val graph = original.toSeq
      .zip(changed.zip(cost))
      .groupMap { case (o, _) => o } { case (_, (c, cost)) => (c, cost) }

    def bfs(source: Char): Map[Char, Long] =
      Iterator
        .iterate((Seq((source, 0)), Map.empty[Char, Long])) { case ((curr, dist) +: toVisit, distances) =>
          graph.getOrElse(curr, Seq.empty).foldLeft(toVisit, distances) { case ((toVisit, dists), (next, d)) =>
            if dists.get(next).exists(_ < dist + d) then (toVisit, dists)
            else (toVisit :+ (next, dist + d), dists + (next -> (dist + d)))
          }
        }
        .dropWhile { case (toVisit, _) => toVisit.nonEmpty }.next()
        .pipe { case (_, distances) => distances }

    val distances = source.distinct.map(s => s -> bfs(s)).toMap

    if source.zip(target).exists { case (s, t) => s != t && !distances(s).contains(t) } then -1
    else source.zip(target).map { case (s, t) => if s == t then 0 else distances(s)(t) }.sum
