package io.github.cosminci.leetcode._200

import io.github.cosminci.utils.TreeNode

object _110_BalancedBinaryTree:

  private def isBalanced(root: TreeNode): Boolean =
    def dfs(node: TreeNode): (Boolean, Int) =
      if node == null then return (true, 0)
      val (leftBalanced, leftHeight)   = dfs(node.left)
      val (rightBalanced, rightHeight) = dfs(node.right)
      val balanced                     = leftBalanced && rightBalanced && math.abs(leftHeight - rightHeight) <= 1
      (balanced, 1 + math.max(leftHeight, rightHeight))

    dfs(root)._1
