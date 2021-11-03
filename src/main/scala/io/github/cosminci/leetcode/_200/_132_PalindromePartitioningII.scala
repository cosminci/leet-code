package io.github.cosminci.leetcode._200

import scala.collection.mutable

object _132_PalindromePartitioningII:
  def main(args: Array[String]): Unit =
    println(minCutBottomUp("ababababababababababababcbabababababababa"))
    println(minCutTopDown("ababababababababababababcbabababababababa"))

  def minCutBottomUp(s: String): Int =
    val isPalindromeDP = Array.ofDim[Boolean](s.length, s.length)
    val minCutUntilIdx = Array.ofDim[Int](s.length)

    (0 until s.length).foreach { endIdx =>
      minCutUntilIdx(endIdx) = endIdx

      (0 to endIdx).foreach { startIdx =>
        if s.charAt(startIdx) == s.charAt(endIdx) &&
          (endIdx - startIdx <= 1 || isPalindromeDP(startIdx + 1)(endIdx - 1))
        then
          isPalindromeDP(startIdx)(endIdx) = true
          minCutUntilIdx(endIdx) =
            if startIdx == 0 then 0
            else math.min(minCutUntilIdx(endIdx), minCutUntilIdx(startIdx - 1) + 1)
      }
    }
    minCutUntilIdx.last

  def minCutTopDown(s: String): Int =
    val mem = mutable.Map.empty[(Int, Int), Int]

    def dfs(startIdx: Int, endIdx: Int): Int =
      if mem.contains((startIdx, endIdx)) then return mem((startIdx, endIdx))

      val result =
        if startIdx == endIdx || isPalindrome(startIdx, endIdx) then 0
        else
          (startIdx until endIdx).map { cutIdx =>
            1 + dfs(startIdx, cutIdx) + dfs(cutIdx + 1, endIdx)
          }.min

      mem.update((startIdx, endIdx), result)
      result

    def isPalindrome(startIdx: Int, endIdx: Int): Boolean =
      var (l, r) = (startIdx, endIdx)
      while l < r do
        if s(l) != s(r) then return false
        l += 1
        r -= 1
      true

    dfs(0, s.length - 1)
