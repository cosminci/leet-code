package com.leetcode.cosminci._1000

object _944_DeleteColsToMakeSorted:

  def minDeletionSize(strs: Array[String]): Int =
    strs.toSeq.transpose.count(l => l != l.sorted)
