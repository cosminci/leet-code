package io.github.cosminci.leetcode._200

import io.github.cosminci.utils.TreeNode

object _112_PathSum:
  def hasPathSum(root: TreeNode, targetSum: Int): Boolean =
    if root == null then return false
    if root.left == null && root.right == null then return root.value == targetSum

    hasPathSum(root.left, targetSum - root.value) ||
    hasPathSum(root.right, targetSum - root.value)
