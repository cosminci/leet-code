package com.leetcode.cosminci._1900

import scala.util.chaining.*

object _1857_LargestColorValueInDirectedGraph:

  def largestPathValue(colors: String, edges: Array[Array[Int]]): Int =
    val inDegree = edges.foldLeft(Map.empty[Int, Int].withDefaultValue(0)) { case (acc, Array(_, v)) =>
      acc.updated(v, acc(v) + 1)
    }
    val graph = edges.foldLeft(Map.empty[Int, Seq[Int]].withDefaultValue(Seq.empty)) { case (graph, Array(u, v)) =>
      graph.updated(u, graph(u) :+ v)
    }
    val toVisit = (0 until colors.length).filter(inDegree(_) == 0)
    val counts  = Map.empty[Int, Array[Int]].withDefaultValue(Array.fill(26)(0))

    Iterator
      .iterate((toVisit, inDegree, counts)) { case (u +: toVisit, inDegree, counts) =>
        val color     = colors(u) - 'a'
        val newCounts = counts.updated(u, counts(u).updated(color, counts(u)(color) + 1))
        graph(u).foldLeft(toVisit, inDegree, newCounts) { case ((toVisit, inDegree, counts), v) =>
          val newCounts   = counts.updated(v, counts(v).zip(counts(u)).map { case (a, b) => a.max(b) })
          val newInDegree = inDegree.updated(v, inDegree(v) - 1)
          val newToVisit  = if (newInDegree(v) == 0) toVisit :+ v else toVisit
          (newToVisit, newInDegree, newCounts)
        }
      }
      .dropWhile { case (toVisit, _, _) => toVisit.nonEmpty }.next()
      .pipe { case (_, inDegree, counts) =>
        if (inDegree.values.sum > 0) -1
        else counts.values.map(_.max).max
      }
