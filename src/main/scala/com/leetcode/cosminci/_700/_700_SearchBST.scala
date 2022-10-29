package com.leetcode.cosminci._700

import com.leetcode.cosminci.utils.TreeNode

object _700_SearchBST:

  @annotation.tailrec
  def searchBST(root: TreeNode, value: Int): TreeNode =
    if root == null then root
    else if root.value == value then root
    else if root.value < value then searchBST(root.right, value)
    else searchBST(root.left, value)
