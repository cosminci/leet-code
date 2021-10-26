package io.github.cosminci.leetcode._200

import io.github.cosminci.utils.TreeNode

import scala.collection.mutable

object _104_MaxDepthOfBinaryTree:

  def maxDepthBFS(root: TreeNode): Int =
    if root == null then return 0
    val queue = mutable.Queue(root)
    var level = 0
    while queue.nonEmpty do
      queue.dequeueAll(_ => true).foreach { n =>
        if n.left != null then queue.enqueue(n.left)
        if n.right != null then queue.enqueue(n.right)
      }
      level += 1
    level

  def maxDepthStackDFS(root: TreeNode): Int =
    if root == null then return 0
    val queue = mutable.Queue((root, 1))
    var max   = 0
    while queue.nonEmpty do
      val (node, depth) = queue.dequeue()
      if node != null then
        max = math.max(depth, max)
        queue.addOne((node.left, depth + 1))
        queue.addOne((node.right, depth + 1))
    max

  def maxDepthRecursiveDFS(root: TreeNode): Int =
    var max = 0
    def dfs(node: TreeNode, depth: Int): Unit =
      if node == null then return
      if depth > max then max = depth
      dfs(node.left, depth + 1)
      dfs(node.right, depth + 1)
    dfs(root, 1)
    max
