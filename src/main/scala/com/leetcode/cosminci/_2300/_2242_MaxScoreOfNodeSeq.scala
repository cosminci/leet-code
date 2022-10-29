package com.leetcode.cosminci._2300

import scala.collection.mutable

object _2242_MaxScoreOfNodeSeq:

  def maximumScore(scores: Array[Int], edges: Array[Array[Int]]): Int =
    val graph = Array.fill(scores.length)(mutable.ListBuffer.empty[Int])
    edges.foreach { case Array(x, y) =>
      graph(x).append(y)
      graph(y).append(x)
    }
    val reducedGraph = graph.map(_.sortBy(scores).takeRight(3))

    val sequenceScores = for
      Array(x, y) <- edges
      nx          <- reducedGraph(x)
      ny          <- reducedGraph(y)
      if nx != ny && nx != y && x != ny
    yield scores(nx) + scores(x) + scores(y) + scores(ny)

    sequenceScores.maxOption.getOrElse(-1)
