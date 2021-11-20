package io.github.cosminci.leetcode._1000

import io.github.cosminci.utils.TreeNode

import scala.collection.mutable

object _919_CompleteBinaryTreeInserter {
  def main(args: Array[String]): Unit = {
    val cbtInserter = new CBTInserter(new TreeNode(1, new TreeNode(2)))
    cbtInserter.insert(3)
    cbtInserter.insert(4)
    val root = cbtInserter.get_root()
    println(root.value)
  }

  class CBTInserter(root: TreeNode) {
    private val tree = mutable.IndexedBuffer.from {
      @annotation.tailrec
      def dfs(idx: Int, values: Seq[TreeNode]): Seq[TreeNode] =
        if (idx >= values.length) values
        else dfs(idx + 1, values ++ Option(values(idx).left) ++ Option(values(idx).right))
      dfs(idx = 0, Seq(root))
    }

    def insert(value: Int): Int = {
      val n = tree.length
      val parent = tree((n - 1) / 2)
      val node = new TreeNode(value)

      if (n % 2 == 1)
        parent.left = node
      else
        parent.right = node
      tree.append(node)

      parent.value
    }

    def get_root(): TreeNode = root
  }
}
