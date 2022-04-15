package io.github.cosminci.leetcode._700

import io.github.cosminci.utils.TreeNode

object _669_TrimABinarySearchTree:

  def trimBST(root: TreeNode, low: Int, high: Int): TreeNode =
    if root == null then null
    else if root.value < low then trimBST(root.right, low, high)
    else if root.value > high then trimBST(root.left, low, high)
    else new TreeNode(root.value, trimBST(root.left, low, high), trimBST(root.right, low, high))
