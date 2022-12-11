package com.leetcode.cosminci._200

import com.leetcode.cosminci.utils.TreeNode

import scala.util.chaining.*

object _124_BinaryTreeMaxPathSum:

  def maxPathSum(root: TreeNode): Int =
    def dfs(node: TreeNode): (Int, Int) =
      if node == null then (0, Int.MinValue)
      else
        val (leftAsLinkMax, leftAsRootMax)   = dfs(node.left).pipe { case (local, global) => (local.max(0), global) }
        val (rightAsLinkMax, rightAsRootMax) = dfs(node.right).pipe { case (local, global) => (local.max(0), global) }
        val selfAsRootMax                    = leftAsLinkMax + rightAsLinkMax + node.value
        (leftAsLinkMax.max(rightAsLinkMax) + node.value, selfAsRootMax.max(leftAsRootMax).max(rightAsRootMax))

    dfs(root).pipe { case (_, max) => max }
