package com.leetcode.cosminci._800

import com.leetcode.cosminci.utils.TreeNode

object _701_InsertIntoBST:
  def insertIntoBST(root: TreeNode, value: Int): TreeNode =
    if root == null then new TreeNode(value)
    else
      if value < root.value then root.left = insertIntoBST(root.left, value)
      else root.right = insertIntoBST(root.right, value)
      root
