package com.leetcode.cosminci._800

import com.leetcode.cosminci.utils.TreeNode

object _783_MinDistanceBetweenBSTNodes:

  def minDiffInBST(root: TreeNode): Int =
    def dfs(node: TreeNode, min: Int, max: Int): Int =
      if node == null then max - min
      else dfs(node.left, min, node.value) min dfs(node.right, node.value, max)

    dfs(root, min = -100_000, max = 100_000)
