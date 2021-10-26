package io.github.cosminci.leetcode._200

import io.github.cosminci.utils.TreeNode

import scala.collection.mutable

object _173_BinarySearchTreeIterator:

  def main(args: Array[String]): Unit =
    val bstIterator = new BSTIterator(new TreeNode(3, new TreeNode(1, null, new TreeNode(2)), new TreeNode(4)))
    println(bstIterator.next())
    println(bstIterator.next())
    println(bstIterator.next())
    println(bstIterator.next())

  class BSTIterator(root: TreeNode):
    private val buffer = mutable.Stack.empty[TreeNode]
    addLeftBranch(root)

    private def addLeftBranch(node: TreeNode): Unit =
      var n = node
      while n != null do
        buffer.push(n)
        n = n.left

    def next(): Int =
      val result = buffer.pop()
      if result.right != null then addLeftBranch(result.right)
      result.value

    def hasNext(): Boolean = buffer.nonEmpty
