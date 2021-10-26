package io.github.cosminci.leetcode._300

import io.github.cosminci.utils.TreeNode

object _222_CountCompleteTreeNodes:

  private def countNodes(root: TreeNode): Int =
    if root == null then return 0

    var (left, right) = (root, root)
    var height        = 0

    while right != null do
      left = left.left
      right = right.right
      height += 1

    if left == null then (1 << height) - 1
    else 1 + countNodes(root.left) + countNodes(root.right)
