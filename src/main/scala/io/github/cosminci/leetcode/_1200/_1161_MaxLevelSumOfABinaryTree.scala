package io.github.cosminci.leetcode._1200

import io.github.cosminci.utils.TreeNode

import scala.collection.mutable

object _1161_MaxLevelSumOfABinaryTree:

  def maxLevelSum(root: TreeNode): Int =
    if root.left == null && root.right == null then return 1

    val toVisit                   = mutable.Queue(root)
    var (level, minLevel, maxSum) = (1, 1, Int.MinValue)

    while toVisit.nonEmpty do
      val levelNodes = toVisit.dequeueAll(_ => true)
      val levelSum   = levelNodes.map(_.value).sum
      if levelSum > maxSum then
        minLevel = level
        maxSum = levelSum
      toVisit.enqueueAll(levelNodes.flatMap(n => List(Option(n.left), Option(n.right)).flatten))
      level += 1

    minLevel
