package io.github.cosminci.leetcode._400

import io.github.cosminci.utils.TreeNode

object _337_HouseRobberIII:
  def rob(root: TreeNode): Int =
    def dfs(node: TreeNode): (Int, Int) =
      if node == null then (0, 0)
      else
        val (leftWith, leftWithout)   = dfs(node.left)
        val (rightWith, rightWithout) = dfs(node.right)
        (node.value + leftWithout + rightWithout, leftWith.max(leftWithout) + rightWith.max(rightWithout))

    val (withRoot, withoutRoot) = dfs(root)
    withRoot max withoutRoot
