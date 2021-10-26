package io.github.cosminci.leetcode._800

object _784_LetterCasePermutation:
  def main(args: Array[String]): Unit =
    println(letterCasePermutation("a1b2"))
    println(letterCasePermutation("abcd"))

  def letterCasePermutation(s: String): List[String] =
    def dfs(idx: Int): List[String] =
      val localChoices =
        if s(idx).isLetter then List(s(idx).toUpper, s(idx).toLower)
        else List(s(idx))

      if idx == s.length - 1 then localChoices.map(_.toString)
      else dfs(idx + 1).flatMap(str => localChoices.map(str.prepended))

    dfs(0)
