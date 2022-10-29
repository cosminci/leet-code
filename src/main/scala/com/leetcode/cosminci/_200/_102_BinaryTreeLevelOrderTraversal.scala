package com.leetcode.cosminci._200

import com.leetcode.cosminci.utils.TreeNode

import scala.collection.mutable

object _102_BinaryTreeLevelOrderTraversal:

  def levelOrder(root: TreeNode): List[List[Int]] =
    Iterator
      .iterate(List(Option(root)).flatten) { prevLevel =>
        prevLevel.flatMap(node => List(Option(node.left), Option(node.right)).flatten)
      }
      .takeWhile(_.nonEmpty)
      .map(_.map(_.value))
      .toList
