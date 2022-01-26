package io.github.cosminci.leetcode._1400

import io.github.cosminci.utils.TreeNode

object _1305_AllElementsInTwoBSTs:
  def getAllElements(root1: TreeNode, root2: TreeNode): List[Int] =
    def elements(node: TreeNode): List[Int] =
      if node == null then List.empty
      else (elements(node.left) :+ node.value) ++ elements(node.right)

    def zip(l1: List[Int], l2: List[Int]): List[Int] =
      if l1.isEmpty then l2
      else if l2.isEmpty then l1
      else if l1.head < l2.head then l1.head +: zip(l1.tail, l2)
      else l2.head +: zip(l1, l2.tail)

    zip(elements(root1), elements(root2))
