package io.github.cosminci.leetcode._1000

import io.github.cosminci.utils.TreeNode

import scala.collection.mutable

object _938_RangeSumOfBST:
  def rangeSumBSTRecursive(root: TreeNode, low: Int, high: Int): Int =
    def dfs(node: TreeNode): Int =
      if node == null then return 0

      if node.value < low then dfs(node.right)
      else if node.value > high then dfs(node.left)
      else if node.value == low then node.value + dfs(node.right)
      else if node.value == high then node.value + dfs(node.left)
      else node.value + dfs(node.left) + dfs(node.right)

    dfs(root)

  def rangeSumBSTIterative(root: TreeNode, low: Int, high: Int): Int =
    val toVisit = mutable.Queue(root)
    var sum     = 0

    while toVisit.nonEmpty do
      val node = toVisit.dequeue()
      if node != null then
        if node.value >= low && node.value <= high then sum += node.value

        if node.value > high then toVisit.enqueue(node.left)
        else if node.value < low then toVisit.enqueue(node.right)
        else
          toVisit.enqueue(node.left)
          toVisit.enqueue(node.right)

    sum
