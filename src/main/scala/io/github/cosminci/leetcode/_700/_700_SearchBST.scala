package io.github.cosminci.leetcode._700

import io.github.cosminci.utils.TreeNode

object _700_SearchBST:

  @annotation.tailrec
  def searchBST(root: TreeNode, value: Int): TreeNode =
    if root == null then root
    else if root.value == value then root
    else if root.value < value then searchBST(root.right, value)
    else searchBST(root.left, value)
