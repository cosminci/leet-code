package io.github.cosminci.leetcode._1000

import io.github.cosminci.utils.TreeNode

import scala.collection.immutable.TreeMap

object _987_VerticalOrderTraversalOfBinaryTree:

  def verticalTraversal(root: TreeNode): List[List[Int]] =
    @annotation.tailrec
    def dfs(curr: Array[(TreeNode, Int, Int)], acc: TreeMap[Int, List[(Int, Int)]]): TreeMap[Int, List[(Int, Int)]] =
      if curr.isEmpty then acc
      else
        val nextToVisit = curr.flatMap { case (node, x, y) =>
          Seq(
            Option(node.left).map(n => (n, x + 1, y - 1)),
            Option(node.right).map(n => (n, x + 1, y + 1))
          ).flatten
        }
        val newVisited = curr.foldLeft(acc) { case (visited, (n, x, y)) =>
          visited.updated(y, (x, n.value) +: visited.getOrElse(y, List.empty))
        }
        dfs(nextToVisit, newVisited)

    dfs(curr = Array((root, 0, 0)), acc = TreeMap.empty[Int, List[(Int, Int)]]).values.toList
      .map(cols => cols.sorted.map { case (_, v) => v })
