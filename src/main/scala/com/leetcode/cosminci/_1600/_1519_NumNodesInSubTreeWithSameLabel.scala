package com.leetcode.cosminci._1600

object _1519_NumNodesInSubTreeWithSameLabel:

  def countSubTrees(n: Int, edges: Array[Array[Int]], labels: String): Array[Int] =
    val graph = edges.foldLeft(Map.empty[Int, Seq[Int]].withDefaultValue(Seq.empty)) { case (graph, Array(a, b)) =>
      graph.updated(a, graph(a) :+ b).updated(b, graph(b) :+ a)
    }

    val result = Array.fill(n)(0)
    def dfs(node: Int, parent: Int): Array[Int] =
      val nodeCounts = Array.tabulate(n)(i => if i == labels(node - 'a') then 1 else 0)
      graph(node).filterNot(_ == parent).map(dfs(_, node)).foreach { childCounts =>
        childCounts.indices.foreach { i =>
          nodeCounts(i) += childCounts(i)
        }
      }
      result(node) = nodeCounts(labels(node) - 'a')
      nodeCounts

    dfs(node = 0, parent = -1)
    result
