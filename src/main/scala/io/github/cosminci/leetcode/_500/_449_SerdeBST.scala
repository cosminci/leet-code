package io.github.cosminci.leetcode._500

import io.github.cosminci.utils.TreeNode

import scala.collection.immutable.Queue

object _449_SerdeBST:
  def main(args: Array[String]): Unit =
    val codec = new Codec
    val bst   = new TreeNode(1, null, new TreeNode(3))
    println(codec.serialize(bst))
    val recovered = codec.deserialize(codec.serialize(bst))
    println(recovered)

  class Codec:
    def serialize(root: TreeNode): String =
      if root == null then "#"
      else s"${root.value},${serialize(root.left)},${serialize(root.right)}"

    def deserialize(data: String): TreeNode =
      val elements = Seq.from(data.split(","))

      def dfs(idx: Int): (TreeNode, Int) =
        if elements(idx) == "#" then (null, idx)
        else
          val (left, leftEndIdx)   = dfs(idx + 1)
          val (right, rightEndIdx) = dfs(leftEndIdx + 1)
          (new TreeNode(elements(idx).toInt, left, right), rightEndIdx)

      dfs(0)._1
