package io.github.cosminci.leetcode._700

import io.github.cosminci.utils.TreeNode

import scala.collection.mutable

object _654_MaxBinaryTree:

  def constructMaximumBinaryTreeRecursive(nums: Array[Int]): TreeNode =
    def dfs(arr: Array[Int]): TreeNode =
      if arr.length == 0 then return null
      val (fh, sh) = arr.splitAt(arr.indexOf(arr.max))
      new TreeNode(sh.head, dfs(fh), dfs(sh.tail))
    dfs(nums)

  def constructMaximumBinaryTreeStack(nums: Array[Int]): TreeNode =
    val stack = mutable.Stack.empty[TreeNode]

    nums.foreach { n =>
      val node = new TreeNode(n)
      while stack.headOption.exists(_.value < n) do node.left = stack.pop()
      if stack.nonEmpty then stack.head.right = node
      stack.push(node)
    }

    stack.lastOption.orNull
