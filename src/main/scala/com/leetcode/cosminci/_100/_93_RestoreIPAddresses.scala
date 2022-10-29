package com.leetcode.cosminci._100

object _93_RestoreIPAddresses:
  def main(args: Array[String]): Unit =
    println(restoreIpAddresses("25525511135"))
    println(restoreIpAddresses("0000"))
    println(restoreIpAddresses("101023"))

  def restoreIpAddresses(str: String): List[String] =
    if str.length < 4 || str.length > 12 then return List.empty

    def isValidBlock(s: String) = s.toInt <= 255 && (s.length == 1 || s.head != '0')

    def blockCombinations(s: String, block: Int) =
      (1 to 3).flatMap { numDigitsToUse =>
        if s.length <= numDigitsToUse || !isValidBlock(s.take(numDigitsToUse)) then Seq.empty
        else
          val (currBlock, remaining) = s.splitAt(numDigitsToUse)
          dfs(remaining, block + 1).map(suffix => s"$currBlock.$suffix")
      }

    def dfs(s: String, block: Int): Seq[String] =
      if s.isEmpty then Seq.empty
      else if block == 4 then if isValidBlock(s) then Seq(s) else Seq.empty
      else blockCombinations(s, block)

    dfs(str, 1).toList
