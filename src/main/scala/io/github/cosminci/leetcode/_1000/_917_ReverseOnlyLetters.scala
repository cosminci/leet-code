package io.github.cosminci.leetcode._1000

object _917_ReverseOnlyLetters:
  def main(args: Array[String]): Unit =
    println(reverseOnlyLetters("ab-cd"))
    println(reverseOnlyLetters("a-bC-dEf-ghIj"))
    println(reverseOnlyLetters("Test1ng-Leet=code-Q!"))

  def reverseOnlyLetters(s: String): String =
    val result = new StringBuilder(s)
    var (l, r) = (0, s.length - 1)
    while l < r do
      if !s(l).isLetter then l += 1
      else if !s(r).isLetter then r -= 1
      else
        val tmp = result(l)
        result(l) = result(r)
        result(r) = tmp
        l += 1
        r -= 1

    result.toString
