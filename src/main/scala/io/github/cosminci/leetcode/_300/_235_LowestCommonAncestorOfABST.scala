package io.github.cosminci.leetcode._300

import io.github.cosminci.utils.TreeNode

object _235_LowestCommonAncestorOfABST:

  def lowestCommonAncestor(root: TreeNode, p: TreeNode, q: TreeNode): TreeNode =
    var node            = root
    val (lower, higher) = (math.min(p.value, q.value), math.max(p.value, q.value))
    while true do
      if node.value > higher then node = node.left
      else if node.value < lower then node = node.right
      else return node
    node
