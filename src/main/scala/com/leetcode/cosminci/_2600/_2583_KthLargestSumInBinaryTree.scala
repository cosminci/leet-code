package com.leetcode.cosminci._2600

import com.leetcode.cosminci.utils.TreeNode
import scala.collection.immutable.TreeSet
import scala.util.chaining.*

object _2583_KthLargestSumInBinaryTree:

  def kthLargestLevelSum(root: TreeNode, k: Int): Long =
    @annotation.tailrec
    def dfs(nodes: Seq[TreeNode], lvl: Int, sums: TreeSet[(Long, Int)]): Long =
      if nodes.isEmpty then if lvl < k then -1 else sums.last.pipe { case (sum, _) => sum }
      else
        val newSums  = (sums + (nodes.map(_.value.toLong).sum -> lvl)).pipe(s => if s.size > k then s.tail else s)
        val newNodes = nodes.flatMap(n => Seq(Option(n.left), Option(n.right)).flatten)
        dfs(newNodes, lvl + 1, newSums)

    dfs(nodes = Seq(root), lvl = 0, sums = TreeSet.empty)
