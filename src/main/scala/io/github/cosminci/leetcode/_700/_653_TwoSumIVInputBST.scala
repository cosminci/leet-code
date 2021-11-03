package io.github.cosminci.leetcode._700

import io.github.cosminci.utils.TreeNode

import scala.collection.mutable

object _653_TwoSumIVInputBST {
  def findTarget(root: TreeNode, k: Int): Boolean = {
    val seen = mutable.Set.empty[Int]
    val toVisit = mutable.Stack.empty[TreeNode]
    var node = root
    while (node != null || toVisit.nonEmpty) {
      while (node != null) {
        toVisit.push(node)
        node = node.left
      }
      node = toVisit.pop()
      if (seen.contains(k - node.value))
        return true
      seen.add(node.value)

      node = node.right
    }
    false
  }
}
