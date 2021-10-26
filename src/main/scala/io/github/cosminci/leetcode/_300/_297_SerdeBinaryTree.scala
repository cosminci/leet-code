package io.github.cosminci.leetcode._300

import io.github.cosminci.utils.TreeNode

object _297_SerdeBinaryTree:
  def main(args: Array[String]): Unit =
    val codec = new Codec
    val node  = new TreeNode(3)
    node.left = new TreeNode(-5)
    node.left.right = new TreeNode(11)
    node.right = new TreeNode(2)
    println(codec.deserialize(codec.serialize(node)).equals(node))

  class Codec:
    def serialize(root: TreeNode): String =
      def dfs(n: TreeNode): List[String] =
        if n == null then List("n")
        else (n.value.toString +: dfs(n.left)) ++ dfs(n.right)
      dfs(root).mkString(",")

    def deserialize(data: String): TreeNode =
      val nodes = data.split(",")
      def dfs(idx: Int): (TreeNode, Int) =
        if nodes(idx) == "n" then return (null, idx)

        val node = new TreeNode(nodes(idx).toInt)

        val (left, indexAfterLeft)   = dfs(idx + 1)
        val (right, indexAfterRight) = dfs(indexAfterLeft + 1)

        node.left = left
        node.right = right

        (node, indexAfterRight)
      dfs(0)._1
