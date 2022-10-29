package com.leetcode.cosminci._200

import com.leetcode.cosminci.utils.TreeNode

object _110_BalancedBinaryTree:

  def isBalanced(root: TreeNode): Boolean =
    def dfs(node: TreeNode): (Boolean, Int) =
      if node == null then return (true, 0)
      val (leftBalanced, leftHeight)   = dfs(node.left)
      val (rightBalanced, rightHeight) = dfs(node.right)
      val balanced                     = leftBalanced && rightBalanced && math.abs(leftHeight - rightHeight) <= 1
      (balanced, 1 + math.max(leftHeight, rightHeight))

    dfs(root)._1
