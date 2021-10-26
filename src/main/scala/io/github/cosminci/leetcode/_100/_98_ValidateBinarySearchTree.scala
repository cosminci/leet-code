package io.github.cosminci.leetcode._100

import io.github.cosminci.utils.TreeNode

object _98_ValidateBinarySearchTree:
  def main(args: Array[String]): Unit =
    println(isValidBST(new TreeNode(Int.MinValue, new TreeNode(Int.MinValue))))

  def isValidBST(root: TreeNode): Boolean =
    def dfs(node: TreeNode, min: Long, max: Long): Boolean =
      if node == null then return true

      if node.value < min || node.value > max then return false

      dfs(node.left, min, node.value - 1L) &&
      dfs(node.right, node.value + 1L, max)

    dfs(root, min = Long.MinValue, max = Long.MaxValue)
