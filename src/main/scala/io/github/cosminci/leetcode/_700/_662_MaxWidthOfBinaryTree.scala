package io.github.cosminci.leetcode._700

import io.github.cosminci.utils.TreeNode

import scala.collection.mutable

object _662_MaxWidthOfBinaryTree:
  private def widthOfBinaryTree(root: TreeNode): Int =
    val toVisit = mutable.Queue((root, 0))
    var max     = 0

    while toVisit.nonEmpty do
      val levelNodes = toVisit.dequeueAll(_ => true)
      max = math.max(max, levelNodes.last._2 - levelNodes.head._2 + 1)
      levelNodes.foreach { case (n, idx) =>
        if n.left != null then toVisit.enqueue((n.left, 2 * idx))
        if n.right != null then toVisit.enqueue((n.right, 2 * idx + 1))
      }

    max
