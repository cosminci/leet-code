package io.github.cosminci.leetcode._1000

import io.github.cosminci.utils.TreeNode

import scala.collection.mutable

object _958_CheckCompletenessOfABinaryTree:
  def main(args: Array[String]): Unit =
    print(isCompleteTree(new TreeNode(1, new TreeNode(2, new TreeNode(4)), new TreeNode(3))))

  private def isCompleteTree(root: TreeNode): Boolean =
    val toVisit   = mutable.Queue(root)
    var foundNull = false

    while toVisit.nonEmpty do
      val node = toVisit.dequeue()
      if node == null then foundNull = true
      else
        if foundNull then return false
        toVisit.enqueue(node.left)
        toVisit.enqueue(node.right)

    true
