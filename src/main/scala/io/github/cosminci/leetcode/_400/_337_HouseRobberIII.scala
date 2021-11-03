package io.github.cosminci.leetcode._400

import io.github.cosminci.utils.TreeNode

object _337_HouseRobberIII:
  def rob(root: TreeNode): Int =
    def dfs(node: TreeNode): (Int, Int) =
      if node == null then return (0, 0)

      val (maxLeftWith, maxLeftWithout)   = dfs(node.left)
      val (maxRightWith, maxRightWithout) = dfs(node.right)

      (
        node.value + maxLeftWithout + maxRightWithout,
        math.max(maxLeftWith, maxLeftWithout) + math.max(maxRightWith, maxRightWithout)
      )

    val (withRoot, withoutRoot) = dfs(root)
    math.max(withRoot, withoutRoot)
