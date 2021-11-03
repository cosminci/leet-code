package io.github.cosminci.leetcode._100

object _17_LetterCombinationsOfAPhoneNumber:
  def main(args: Array[String]): Unit =
    println(letterCombinationsRecursive("7"))
    println(letterCombinationsIterative("7"))

  private val digitToLetters = Map(
    '2' -> List("a", "b", "c"),
    '3' -> List("d", "e", "f"),
    '4' -> List("g", "h", "i"),
    '5' -> List("j", "k", "l"),
    '6' -> List("m", "n", "o"),
    '7' -> List("p", "q", "r", "s"),
    '8' -> List("t", "u", "v"),
    '9' -> List("w", "x", "y", "z")
  )

  def letterCombinationsRecursive(digits: String): List[String] =
    if digits.length == 0 then return List.empty

    def dfs(idx: Int): Seq[String] =
      if idx == digits.length - 1 then digitToLetters(digits(idx))
      else digitToLetters(digits(idx)).flatMap(prefix => dfs(idx + 1).map(_.prependedAll(prefix)))

    dfs(0).toList

  def letterCombinationsIterative(digits: String): List[String] =
    if digits.length == 0 then return List.empty
    digits.tail.foldLeft(digitToLetters(digits.head)) { case (combinations, digit) =>
      digitToLetters(digit).flatMap(letter => combinations.map(_.appendedAll(letter)))
    }
