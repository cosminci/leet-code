package io.github.cosminci.leetcode._300

import io.github.cosminci.utils.TreeNode

object _236_LowestCommonAncestor:

  def lowestCommonAncestor(root: TreeNode, p: TreeNode, q: TreeNode): TreeNode =
    if root == null then return null

    if root.value == p.value || root.value == q.value then return root

    val right = lowestCommonAncestor(root.right, p, q)
    val left  = lowestCommonAncestor(root.left, p, q)

    if right != null && left != null then root
    else if right != null then right
    else left
