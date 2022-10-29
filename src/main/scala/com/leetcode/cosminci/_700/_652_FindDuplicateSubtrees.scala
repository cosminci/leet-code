package com.leetcode.cosminci._700

import com.leetcode.cosminci.utils.TreeNode

import scala.collection.mutable

object _652_FindDuplicateSubtrees:
  def findDuplicateSubtrees(root: TreeNode): List[TreeNode] =
    val counts = mutable.Map.empty[String, mutable.ListBuffer[TreeNode]]

    def convert(node: TreeNode): String =
      if node == null then "404"
      else
        val serial = s"${node.value}#${convert(node.left)}#${convert(node.right)}"
        counts.getOrElseUpdate(serial, mutable.ListBuffer.empty).append(node)
        serial

    convert(root)
    counts.values.collect { case nodes if nodes.length > 1 => nodes.head }.toList
