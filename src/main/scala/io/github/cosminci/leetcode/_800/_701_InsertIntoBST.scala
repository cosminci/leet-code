package io.github.cosminci.leetcode._800

import io.github.cosminci.utils.TreeNode

object _701_InsertIntoBST:
  private def insertIntoBST(root: TreeNode, value: Int): TreeNode =
    if root == null then new TreeNode(value)
    else
      if value < root.value then root.left = insertIntoBST(root.left, value)
      else root.right = insertIntoBST(root.right, value)
      root
