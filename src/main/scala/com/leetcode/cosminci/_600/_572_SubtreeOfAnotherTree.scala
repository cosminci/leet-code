package com.leetcode.cosminci._600

import com.leetcode.cosminci.utils.TreeNode

object _572_SubtreeOfAnotherTree:
  def isSubtree(root: TreeNode, subRoot: TreeNode): Boolean =
    if root == null && subRoot == null then return true
    if root == null || subRoot == null then return false
    areIdentical(root, subRoot) ||
    isSubtree(root.left, subRoot) || isSubtree(root.right, subRoot)

  def areIdentical(n1: TreeNode, n2: TreeNode): Boolean =
    if n1 == null && n2 == null then return true
    if n1 == null || n2 == null then return false
    n1.value == n2.value && areIdentical(n1.left, n2.left) && areIdentical(n1.right, n2.right)
