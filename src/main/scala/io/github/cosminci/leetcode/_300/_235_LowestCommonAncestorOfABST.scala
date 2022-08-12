package io.github.cosminci.leetcode._300

import io.github.cosminci.utils.TreeNode

object _235_LowestCommonAncestorOfABST:

  def lowestCommonAncestor(root: TreeNode, p: TreeNode, q: TreeNode): TreeNode =
    val (lower, higher) = (p.value min q.value, p.value max q.value)
    Iterator
      .iterate(root)(node => if node.value > higher then node.left else node.right)
      .dropWhile(node => node.value > higher || node.value < lower)
      .next()
