package io.github.cosminci.leetcode._600

import io.github.cosminci.utils.Node

import scala.collection.mutable

object _589_NAryTreePreorderTraversal:
  def preorderRecursive(root: Node): List[Int] =
    if root == null then return List.empty
    if root.children.isEmpty then return List(root.value)

    root.value +: (preorderRecursive(root.children.head) ++ root.children.tail.flatMap(preorderRecursive))

  def preorderIterative(root: Node): List[Int] =
    if root == null then return List.empty
    val result  = mutable.ListBuffer.empty[Int]
    val toVisit = mutable.Stack(root)

    while toVisit.nonEmpty do
      val node = toVisit.pop()
      result.addOne(node.value)
      node.children.reverse.foreach(toVisit.push)
    result.toList
