package io.github.cosminci.leetcode._200

import io.github.cosminci.utils.TreeNode

import scala.collection.mutable

object _102_BinaryTreeLevelOrderTraversal:
  def main(args: Array[String]): Unit =
    val root = new TreeNode(3, new TreeNode(9), new TreeNode(20, new TreeNode(15), new TreeNode(7)))
    println(levelOrder(root))
    println(levelOrderNoMaxDepth(root))

  private def levelOrder(root: TreeNode): List[List[Int]] =
    if root == null then return List.empty

    val toVisit = mutable.Queue((root, 0))
    val result  = Array.fill(maxDepth(root))(mutable.ListBuffer.empty[Int])
    while toVisit.nonEmpty do
      val (node, depth) = toVisit.dequeue()
      result(depth).addOne(node.value)
      if node.left != null then toVisit.enqueue((node.left, depth + 1))
      if node.right != null then toVisit.enqueue((node.right, depth + 1))
    result.map(_.toList).toList

  private def maxDepth(root: TreeNode): Int =
    if root == null then return 0
    1 + math.max(maxDepth(root.left), maxDepth(root.right))

  private def levelOrderNoMaxDepth(root: TreeNode): List[List[Int]] =
    if root == null then return List.empty

    val toVisit = mutable.ListBuffer(root)
    val result  = mutable.ListBuffer.empty[List[Int]]
    while toVisit.nonEmpty do
      val prevToVisit = toVisit.toList
      result.addOne(prevToVisit.map(_.value))
      toVisit.clear()
      toVisit.addAll(prevToVisit.flatMap { node =>
        List(Option(node.left), Option(node.right)).flatten
      })
    result.toList
