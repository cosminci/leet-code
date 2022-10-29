package com.leetcode.cosminci._100

object _17_LetterCombinationsOfAPhoneNumber:
  def main(args: Array[String]): Unit =
    println(letterCombinationsRecursive("7"))
    println(letterCombinationsIterative("7"))

  private val digitToLetters = Map(
    '2' -> Seq("a", "b", "c"),
    '3' -> Seq("d", "e", "f"),
    '4' -> Seq("g", "h", "i"),
    '5' -> Seq("j", "k", "l"),
    '6' -> Seq("m", "n", "o"),
    '7' -> Seq("p", "q", "r", "s"),
    '8' -> Seq("t", "u", "v"),
    '9' -> Seq("w", "x", "y", "z")
  )

  def letterCombinationsRecursive(digits: String): List[String] =
    def dfs(idx: Int): Seq[String] =
      if idx == digits.length - 1 then digitToLetters(digits(idx))
      else for
        letter <- digitToLetters(digits(idx))
        combi  <- dfs(idx + 1)
      yield combi.prependedAll(letter)

    if digits.isEmpty then List.empty else dfs(idx = 0).toList

  def letterCombinationsIterative(digits: String): List[String] =
    if digits.isEmpty then List.empty
    else digits.tail
      .foldLeft(digitToLetters(digits.head)) { (combinations, digit) =>
        for
          letter <- digitToLetters(digit)
          combi  <- combinations
        yield combi.appendedAll(letter)
      }.toList
