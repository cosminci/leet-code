package com.leetcode.cosminci._200

import com.leetcode.cosminci.utils.TreeNode

import scala.collection.mutable

object _107_BinaryTreeLevelOrderTraversalII:
  def levelOrderBottom(root: TreeNode): List[List[Int]] =
    if root == null then return List.empty

    val results = mutable.ListBuffer.empty[List[Int]]
    val toVisit = mutable.Queue(root)

    while toVisit.nonEmpty do
      val levelNodes = toVisit.dequeueAll(_ => true)
      results.append(levelNodes.map(_.value).toList)
      levelNodes.foreach { n =>
        if n.left != null then toVisit.enqueue(n.left)
        if n.right != null then toVisit.enqueue(n.right)
      }

    results.reverse.toList
