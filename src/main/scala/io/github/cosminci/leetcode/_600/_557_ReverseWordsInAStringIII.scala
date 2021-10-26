package io.github.cosminci.leetcode._600

object _557_ReverseWordsInAStringIII:
  def reverseWords(s: String): String =
    val result   = new StringBuilder
    val currWord = new StringBuilder
    s.foreach { char =>
      if char == ' ' then
        result.append(currWord.reverse.toString()).append(' ')
        currWord.clear()
      else currWord.append(char)
    }
    result.append(currWord.reverse.toString()).toString()
