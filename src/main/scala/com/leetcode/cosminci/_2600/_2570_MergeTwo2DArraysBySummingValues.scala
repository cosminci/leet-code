package com.leetcode.cosminci._2600

object _2570_MergeTwo2DArraysBySummingValues:

  def mergeArrays(nums1: Array[Array[Int]], nums2: Array[Array[Int]]): Array[Array[Int]] =
    @annotation.tailrec
    def dfs(i: Int, j: Int, result: Array[Array[Int]]): Array[Array[Int]] =
      if i == nums1.length && j == nums2.length then result
      else if i == nums1.length then dfs(i, j + 1, result :+ nums2(j))
      else if j == nums2.length then dfs(i + 1, j, result :+ nums1(i))
      else
        val Array(id1, value1) = nums1(i)
        val Array(id2, value2) = nums2(j)
        if id1 == id2 then dfs(i + 1, j + 1, result :+ Array(id1, value1 + value2))
        else if id1 < id2 then dfs(i + 1, j, result :+ nums1(i))
        else dfs(i, j + 1, result :+ nums2(j))

    dfs(i = 0, j = 0, result = Array.empty)
