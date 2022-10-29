package com.leetcode.cosminci._2400

import com.leetcode.cosminci.utils.TreeNode

object _2385_AmountTimeToInfectBinaryTree:

  def amountOfTime(root: TreeNode, start: Int): Int =
    def processLevel(graph: Map[Int, Array[Int]], nodes: Seq[TreeNode]): (Map[Int, Array[Int]], Seq[TreeNode]) =
      val childNodes = nodes.map(n => Seq(Option(n.left), Option(n.right)).flatten)
      val updatedGraph = nodes.zip(childNodes).foldLeft(graph) { case (graph, (parent, children)) =>
        children.foldLeft(graph) { (graph, child) =>
          graph
            .updated(parent.value, graph(parent.value) :+ child.value)
            .updated(child.value, graph(child.value) :+ parent.value)
        }
      }
      (updatedGraph, childNodes.flatten)

    val emptyGraph = Map.empty[Int, Array[Int]].withDefaultValue(Array.empty[Int])
    val graph = Iterator
      .iterate((emptyGraph, Seq(root)))(processLevel)
      .dropWhile { case (_, level) => level.nonEmpty }
      .next()._1

    def dfs(parent: Int, node: Int): Int =
      graph(node)
        .filter(_ != parent)
        .map(child => 1 + dfs(node, child))
        .maxOption
        .getOrElse(0)

    dfs(parent = -1, node = start)
