package io.github.cosminci.leetcode._700

import io.github.cosminci.utils.TreeNode

object _669_TrimABinarySearchTree:
  def trimBST(root: TreeNode, low: Int, high: Int): TreeNode =
    if root == null then return null

    if root.value < low then return trimBST(root.right, low, high)
    if root.value > high then return trimBST(root.left, low, high)

    root.left = trimBST(root.left, low, high)
    root.right = trimBST(root.right, low, high)

    root
