package io.github.cosminci.leetcode._100

import io.github.cosminci.utils.TreeNode

import scala.collection.mutable

object _94_BinaryTreeInOrderTraversal:
  def inorderTraversalRecursive(root: TreeNode): List[Int] =
    if root == null then List.empty
    else inorderTraversalRecursive(root.left).appended(root.value).appendedAll(inorderTraversalRecursive(root.right))

  def inorderTraversalDFS(root: TreeNode): List[Int] = {
    val toVisit = mutable.Stack.empty[TreeNode]
    val result = mutable.ListBuffer.empty[Int]

    var currentParent = root
    while (currentParent != null || toVisit.nonEmpty) {
      while (currentParent != null) {
        toVisit.push(currentParent)
        currentParent = currentParent.left
      }
      currentParent = toVisit.pop()
      result.addOne(currentParent.value)
      currentParent = currentParent.right
    }
    result.toList
  }
