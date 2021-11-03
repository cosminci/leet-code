package io.github.cosminci.leetcode._900

import io.github.cosminci.utils.TreeNode

object _814_BinaryTreePruning:

  def pruneTree(root: TreeNode): TreeNode =
    def dfs(node: TreeNode): TreeNode =
      if node == null then return null
      node.left = dfs(node.left)
      node.right = dfs(node.right)
      if node.left == null && node.right == null && node.value == 0 then null else node
    dfs(root)
