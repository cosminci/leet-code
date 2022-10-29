package com.leetcode.cosminci._1400

import com.leetcode.cosminci.utils.TreeNode

object _1302_DeepestLeavesSum:

  def deepestLeavesSum(root: TreeNode): Int =
    def dfs(node: TreeNode, depth: Int): (Int, Int) =
      if node == null then (0, depth)
      else if node.left == null && node.right == null then (node.value, depth)
      else
        val (lSum, lDepth) = dfs(node.left, depth + 1)
        val (rSum, rDepth) = dfs(node.right, depth + 1)
        if lDepth == rDepth then (lSum + rSum, lDepth)
        else if lDepth > rDepth then (lSum, lDepth)
        else (rSum, rDepth)

    dfs(root, depth = 0)._1
