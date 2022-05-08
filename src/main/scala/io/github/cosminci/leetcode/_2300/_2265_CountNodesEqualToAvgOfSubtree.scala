package io.github.cosminci.leetcode._2300

import io.github.cosminci.utils.TreeNode

object _2265_CountNodesEqualToAvgOfSubtree:

  def averageOfSubtree(root: TreeNode): Int =
    def dfs(node: TreeNode): (Int, Int, Int) =
      if node == null then (0, 0, 0)
      else
        val (lSum, lCount, lResult) = dfs(node.left)
        val (rSum, rCount, rResult) = dfs(node.right)

        val sum    = lSum + rSum + node.value
        val count  = lCount + rCount + 1
        val result = Option.when(sum / count == node.value)(1).getOrElse(0)

        (sum, count, lResult + rResult + result)

    dfs(root)._3
