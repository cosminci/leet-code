package io.github.cosminci.leetcode._200

import io.github.cosminci.utils.TreeNode

import scala.collection.mutable

object _199_BinaryTreeRightSideView:
  def rightSideView(root: TreeNode): List[Int] =
    val results = mutable.ListBuffer.empty[Int]

    def dfs(node: TreeNode, level: Int): Unit =
      if node == null then return
      if level >= results.length then results.append(node.value)
      dfs(node.right, level + 1)
      dfs(node.left, level + 1)

    dfs(root, 0)
    results.toList

  def rightSideViewLevelOrderTraversal(root: TreeNode): List[Int] =
    if root == null then return List.empty

    val results = mutable.ListBuffer.empty[Int]
    val toVisit = mutable.Queue(root)

    while toVisit.nonEmpty do
      val levelNodes = toVisit.dequeueAll(_ => true)
      results.append(levelNodes.last.value)
      levelNodes.foreach { n =>
        if n.left != null then toVisit.enqueue(n.left)
        if n.right != null then toVisit.enqueue(n.right)
      }

    results.toList
