package com.leetcode.cosminci._200

import com.leetcode.cosminci.utils.TreeNode

object _108_ConvertSortedArrayToBST:
  def main(args: Array[String]): Unit =
    val root = sortedArrayToBST(Array(1, 2, 3, 4))
    println(root)

  def sortedArrayToBST(nums: Array[Int]): TreeNode =
    if nums.isEmpty then return null

    val (left, right) = nums.splitAt(nums.length / 2)
    new TreeNode(right.head, sortedArrayToBST(left), sortedArrayToBST(right.tail))
