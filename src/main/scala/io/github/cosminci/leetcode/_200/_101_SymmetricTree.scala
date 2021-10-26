package io.github.cosminci.leetcode._200

import io.github.cosminci.utils.TreeNode

import scala.collection.mutable

object _101_SymmetricTree:
  private def isSymmetric(root: TreeNode): Boolean =
    val toVisit = mutable.Queue(root.left, root.right)

    while toVisit.nonEmpty do
      val (left, right) = (toVisit.dequeue(), toVisit.dequeue())

      if left == null ^ right == null then return false

      if left != null then
        if left.value != right.value then return false
        toVisit.enqueue(left.left)
        toVisit.enqueue(right.right)
        toVisit.enqueue(left.right)
        toVisit.enqueue(right.left)

    true
