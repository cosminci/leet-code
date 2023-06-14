package com.leetcode.cosminci._600

import com.leetcode.cosminci.utils.TreeNode

object _530_MinAbsoluteDiffInBST:

  def getMinimumDifference(root: TreeNode): Int =
    def traverse(node: TreeNode): List[Int] = node match
      case null => Nil
      case _    => traverse(node.left) ++ List(node.value) ++ traverse(node.right)

    traverse(root).sliding(2).map(l => l.last - l.head).min
