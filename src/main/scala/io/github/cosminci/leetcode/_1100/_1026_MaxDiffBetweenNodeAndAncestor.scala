package io.github.cosminci.leetcode._1100

import io.github.cosminci.utils.TreeNode

object _1026_MaxDiffBetweenNodeAndAncestor:
  def maxAncestorDiff(root: TreeNode): Int =
    def dfs(min: Int, max: Int, node: TreeNode): Int =
      if node == null then max - min
      else dfs(node.value.min(min), node.value.max(max), node.left)
        .max(dfs(node.value.min(min), node.value.max(max), node.right))

    dfs(root.value, root.value, root)
