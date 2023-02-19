package com.leetcode.cosminci._200

import com.leetcode.cosminci.utils.TreeNode

import scala.util.chaining.*

object _103_BinaryTreeZigzagLevelOrderTraversal:

  def zigzagLevelOrder(root: TreeNode): List[List[Int]] =
    Iterator
      .iterate((Seq.empty[List[Int]], List(Option(root)).flatten, false)) { case (result, level, reverse) =>
        val toAppend  = if reverse then level.map(_.value).reverse else level.map(_.value)
        val nextLevel = level.flatMap(n => Seq(Option(n.left), Option(n.right)).flatten)
        (result :+ toAppend, nextLevel, !reverse)
      }
      .dropWhile { case (_, level, _) => level.nonEmpty }.next()
      .pipe { case (result, _, _) => result.toList }
