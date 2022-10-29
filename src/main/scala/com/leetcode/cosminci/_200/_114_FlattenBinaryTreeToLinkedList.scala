package com.leetcode.cosminci._200

import com.leetcode.cosminci.utils.TreeNode

import scala.collection.mutable

object _114_FlattenBinaryTreeToLinkedList:

  def flatten(root: TreeNode): Unit =
    Iterator
      .iterate((new TreeNode(_value = 0, _left = null, _right = null), Seq.empty ++ Option(root))) {
        case (prev, stack) =>
          val curr +: remaining = stack
          val newStack = Option(curr.left) ++: Option(curr.right) ++: remaining
          prev.right = curr
          curr.left = null
          (curr, newStack)
      }
      .dropWhile { case (_, stack) => stack.nonEmpty }
      .next()
