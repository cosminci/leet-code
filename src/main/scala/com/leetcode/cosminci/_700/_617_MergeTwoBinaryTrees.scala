package com.leetcode.cosminci._700

import com.leetcode.cosminci.utils.TreeNode

import scala.collection.mutable

object _617_MergeTwoBinaryTrees:
  def mergeTreesRecursive(root1: TreeNode, root2: TreeNode): TreeNode =
    if root1 == null then return root2
    if root2 == null then return root1

    root1.value += root2.value
    root1.left = mergeTreesRecursive(root1.left, root2.left)
    root1.right = mergeTreesRecursive(root1.right, root2.right)
    root1

  def mergeTreesIterative(root1: TreeNode, root2: TreeNode): TreeNode =
    if root1 == null then return root2
    if root2 == null then return root1

    val toVisit = mutable.Stack((root1, root2))
    while toVisit.nonEmpty do
      val (n1, n2) = toVisit.pop()
      n1.value += n2.value

      if n1.left != null && n2.left != null then toVisit.push((n1.left, n2.left))
      else if n1.left == null then n1.left = n2.left

      if n1.right != null && n2.right != null then toVisit.push((n1.right, n2.right))
      if n1.right == null then n1.right = n2.right

    root1
