package io.github.cosminci.leetcode._500

import io.github.cosminci.utils.TreeNode

object _450_DeleteNodeInABST:

  def deleteNode(root: TreeNode, key: Int): TreeNode =
    if root == null then root
    else if root.value < key then
      root.right = deleteNode(root.right, key)
      root
    else if root.value > key then
      root.left = deleteNode(root.left, key)
      root
    else if root.left == null then root.right
    else if root.right == null then root.left
    else
      var leftInsertion = root.right
      while leftInsertion.left != null do leftInsertion = leftInsertion.left
      leftInsertion.left = root.left
      root.right
