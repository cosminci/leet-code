package com.leetcode.cosminci._2400

object _2374_NodeWithHighestEdgeScore:

  def edgeScore(edges: Array[Int]): Int =
    edges.indices
      .groupMapReduce(edges)(_.toLong)(_ + _)
      .maxBy { case (node, edgeScore) => (edgeScore, -node) }
      ._1
