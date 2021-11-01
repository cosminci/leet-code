package io.github.cosminci.leetcode._900

object _880_DecodedStringAtIndex:
  def main(args: Array[String]): Unit =
    println(decodeAtIndex("leet2code3", 10))
    println(decodeAtIndex("ha22", 5))
    println(decodeAtIndex("a2345678999999999999999", 1))

  private def decodeAtIndex(s: String, k: Int): String =
    @annotation.tailrec
    def dfs(length: Long, idx: Int, k: Long): String =
      if s(idx).isDigit then dfs(length / (s(idx) - '0'), idx - 1, k % length)
      else if k % length != 0 then dfs(length - 1, idx - 1, k % length)
      else s(idx).toString

    val decodedLength = s.foldLeft(0L) { (l, char) =>
      Option.when(char.isLetter)(l + 1).getOrElse(l * (char - '0'))
    }

    dfs(decodedLength, s.length - 1, k)
