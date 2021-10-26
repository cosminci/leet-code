package io.github.cosminci.leetcode._100

object _5_LongestPalindromicSubstring:
  def main(args: Array[String]): Unit =
    println(longestPalindromeDP("aaaaaa"))
    println(longestPalindromeExpand("bb"))

  private def longestPalindromeExpand(s: String): String =
    var max = ""
    s.indices.foreach { i =>
      val maxOdd   = expand(s, i, i)
      val maxEven  = expand(s, i, i + 1)
      val localMax = if maxEven.length > maxOdd.length then maxEven else maxOdd
      if localMax.length > max.length then max = localMax
    }
    max

  private def expand(s: String, left: Int, right: Int) =
    var l   = left
    var r   = right
    var max = ""
    while l >= 0 && r < s.length && s.charAt(l) == s.charAt(r) do
      max = s.substring(l, r + 1)
      l -= 1
      r += 1
    max

  private def longestPalindromeDP(s: String): String =
    val n   = s.length
    val dp  = Array.ofDim[Boolean](n, n)
    var max = (0, 0)

    (0 until n).foreach { endIdx =>
      (0 until endIdx).foreach { startIdx =>
        if s.charAt(startIdx) == s.charAt(endIdx) && (endIdx - startIdx <= 1 || dp(startIdx + 1)(endIdx - 1)) then
          dp(startIdx)(endIdx) = true
          if endIdx - startIdx + 1 > max._2 - max._1 then max = (startIdx, endIdx + 1)
      }
    }
    s.substring(max._1, max._2)
