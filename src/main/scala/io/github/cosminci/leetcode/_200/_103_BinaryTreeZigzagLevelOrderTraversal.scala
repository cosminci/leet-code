package io.github.cosminci.leetcode._200

import io.github.cosminci.utils.TreeNode

import scala.collection.mutable

object _103_BinaryTreeZigzagLevelOrderTraversal:
  private def zigzagLevelOrder(root: TreeNode): List[List[Int]] =
    if root == null then return List.empty

    val results = mutable.ListBuffer.empty[List[Int]]
    val toVisit = mutable.Queue(root)
    var reverse = false

    while toVisit.nonEmpty do
      val levelNodes  = toVisit.dequeueAll(_ => true)
      val levelValues = levelNodes.map(_.value)
      results.append((if reverse then levelValues.reverse else levelValues).toList)
      levelNodes.foreach { n =>
        if n.left != null then toVisit.enqueue(n.left)
        if n.right != null then toVisit.enqueue(n.right)
      }
      reverse = !reverse

    results.toList
