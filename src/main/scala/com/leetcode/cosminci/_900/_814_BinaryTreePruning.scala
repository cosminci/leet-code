package com.leetcode.cosminci._900

import com.leetcode.cosminci.utils.TreeNode

object _814_BinaryTreePruning:

  def pruneTree(root: TreeNode): TreeNode =
    def dfs(node: TreeNode): TreeNode =
      if node == null then null
      else
        val (left, right) = (dfs(node.left), dfs(node.right))
        Option.when(left != null || right != null || node.value == 1)(new TreeNode(node.value, left, right)).orNull

    dfs(root)
