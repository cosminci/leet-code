package io.github.cosminci.leetcode._900

import io.github.cosminci.utils.TreeNode

object _865_SmallestSubTreeWithAllTheDeepestNodes:
  private def subtreeWithAllDeepest(root: TreeNode): TreeNode =
    def dfs(node: TreeNode): (TreeNode, Int) =
      if node == null then return (node, 0)

      val (left, leftDepth)   = dfs(node.left)
      val (right, rightDepth) = dfs(node.right)

      if leftDepth < rightDepth then (right, rightDepth + 1)
      else if leftDepth > rightDepth then (left, leftDepth + 1)
      else (node, leftDepth + 1)
    dfs(root)._1
