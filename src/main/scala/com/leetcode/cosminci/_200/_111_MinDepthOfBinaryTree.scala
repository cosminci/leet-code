package com.leetcode.cosminci._200

import com.leetcode.cosminci.utils.TreeNode

import scala.collection.mutable

object _111_MinDepthOfBinaryTree:

  def minDepth(root: TreeNode): Int =
    var level = 0
    if root == null then return level
    val toVisit = mutable.Queue(root)
    while toVisit.nonEmpty do

      level += 1
      toVisit.dequeueAll(_ => true).foreach { n =>
        if n.left == null && n.right == null then return level
        if n.left != null then toVisit.enqueue(n.left)
        if n.right != null then toVisit.enqueue(n.right)
      }

    level
