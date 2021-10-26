package io.github.cosminci.leetcode._300

import io.github.cosminci.utils.TreeNode

import scala.collection.mutable

object _230_KthSmallestElementInBST:
  def main(args: Array[String]): Unit =
    println(kthSmallestIterative(new TreeNode(1, new TreeNode(0)), 1))

  def kthSmallestIterative(root: TreeNode, k: Int): Int =
    var count   = 0
    var node    = root
    val toVisit = mutable.Stack.empty[TreeNode]

    while true do
      while node != null do
        toVisit.push(node)
        node = node.left
      node = toVisit.pop()
      count += 1
      if count == k then return node.value
      node = node.right

    return -1

  def kthSmallestRecursive(root: TreeNode, k: Int): Int =
    var count  = 0
    var result = Int.MinValue

    def dfs(node: TreeNode): Unit =
      if count >= k || node == null then return
      dfs(node.left)
      count += 1
      if count == k then
        result = node.value
        return
      dfs(node.right)

    dfs(root)
    result
