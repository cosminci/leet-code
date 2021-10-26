package io.github.cosminci.leetcode._700

import io.github.cosminci.utils.TreeNode

import scala.collection.mutable

object _637_AverageOfLevelsInBinaryTree:
  private def averageOfLevels(root: TreeNode): Array[Double] =
    val toVisit = mutable.Queue(root)
    var results = mutable.ListBuffer.empty[Double]

    while toVisit.nonEmpty do
      val levelNodes = toVisit.dequeueAll(_ => true)
      levelNodes.foreach { n =>
        if n.left != null then toVisit.enqueue(n.left)
        if n.right != null then toVisit.enqueue(n.right)
      }
      results.append(levelNodes.map(_.value.toDouble).sum / levelNodes.size)

    results.toArray
