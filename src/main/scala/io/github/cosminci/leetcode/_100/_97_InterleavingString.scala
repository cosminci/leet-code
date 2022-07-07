package io.github.cosminci.leetcode._100

import scala.collection.mutable

object _97_InterleavingString:

  def isInterleave(s1: String, s2: String, s3: String): Boolean =
    val mem = mutable.Map.empty[(Int, Int, Int), Boolean]

    def dfs(i: Int, j: Int, k: Int): Boolean = mem.getOrElseUpdate((i, j, k),
      if k == s3.length then true
      else if i == s1.length then s2.substring(j) == s3.substring(k)
      else if j == s2.length then s1.substring(i) == s3.substring(k)
      else if s1(i) == s3(k) && s2(j) != s3(k) then dfs(i + 1, j, k + 1)
      else if s1(i) != s3(k) && s2(j) == s3(k) then dfs(i, j + 1, k + 1)
      else if s1(i) == s3(k) && s2(j) == s3(k) then dfs(i + 1, j, k + 1) || dfs(i, j + 1, k + 1)
      else false
    )

    (s1.length + s2.length == s3.length) && dfs(i = 0, j = 0, k = 0)
