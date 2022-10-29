package com.leetcode.cosminci._900

import com.leetcode.cosminci.utils.TreeNode

object _894_AllPossibleFullBinaryTrees:
  def main(args: Array[String]): Unit =
    Seq(3, 5, 7, 19).foreach(n => println(allPossibleFBT(n).length))

  def allPossibleFBT(n: Int): List[TreeNode] =
    if n % 2 == 0 then List.empty
    else if n == 1 then List(new TreeNode(0))
    else
      for
        leftNodeCount <- (1 until n by 2).toList
        leftChild     <- allPossibleFBT(leftNodeCount)
        rightChild    <- allPossibleFBT(n - leftNodeCount - 1)
      yield new TreeNode(0, leftChild, rightChild)
