package com.leetcode.cosminci._1100

import com.leetcode.cosminci.utils.TreeNode

import scala.collection.mutable

object _1008_ConstructBSTFromPreorderTraversal:
  def main(args: Array[String]): Unit =
    val root = bstFromPreorder(Array(8, 5, 1, 7, 10, 12))
    println(root)

  def bstFromPreorder(preorder: Array[Int]): TreeNode =
    val root = new TreeNode(preorder.head)

    val stack = mutable.Stack(root)
    preorder.tail.foreach { value =>
      val node = new TreeNode(value)
      if value < stack.head.value then stack.head.left = node
      else stack.popWhile(_.value < value).last.right = node
      stack.push(node)
    }

    root
