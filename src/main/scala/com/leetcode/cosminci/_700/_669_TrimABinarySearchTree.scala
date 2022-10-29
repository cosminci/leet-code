package com.leetcode.cosminci._700

import com.leetcode.cosminci.utils.TreeNode

object _669_TrimABinarySearchTree:

  def trimBST(root: TreeNode, low: Int, high: Int): TreeNode =
    if root == null then null
    else if root.value < low then trimBST(root.right, low, high)
    else if root.value > high then trimBST(root.left, low, high)
    else new TreeNode(root.value, trimBST(root.left, low, high), trimBST(root.right, low, high))
