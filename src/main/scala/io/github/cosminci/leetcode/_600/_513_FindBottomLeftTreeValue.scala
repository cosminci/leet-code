package io.github.cosminci.leetcode._600

import io.github.cosminci.utils.TreeNode

import scala.collection.mutable

object _513_FindBottomLeftTreeValue:
  def findBottomLeftValue(root: TreeNode): Int =
    var node  = root
    val queue = mutable.Queue(node)

    while queue.nonEmpty do
      node = queue.dequeue()
      val children = Seq.empty ++ Option(node.right) ++ Option(node.left)
      children.foreach(queue.enqueue)

    node.value
