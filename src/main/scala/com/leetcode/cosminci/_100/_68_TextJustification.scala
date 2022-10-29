package com.leetcode.cosminci._100

object _68_TextJustification:
  def main(args: Array[String]): Unit =
    fullJustify(Array("This", "is", "an", "example", "of", "text", "justification."), 16).foreach(println)
    fullJustify(Array("What", "must", "be", "acknowledgment", "shall", "be"), 16).foreach(println)

  def fullJustify(words: Array[String], maxWidth: Int): List[String] =
    val (completeLines, lastLine) = words.foldLeft((List.empty[List[String]], List.empty[String])) {
      case ((completeLines, currentLine), word) =>
        if currentLine.size + currentLine.map(_.length).sum + word.length > maxWidth then
          (completeLines :+ currentLine, List(word))
        else (completeLines, currentLine :+ word)
    }

    completeLines.map { lineWords =>
      val spacesToInsert = maxWidth - lineWords.map(_.length).sum
      if lineWords.size == 1 then s"${lineWords.head}${" " * spacesToInsert}"
      else
        val baseSpaces =
          if lineWords.size == 1 then maxWidth - lineWords.head.length else spacesToInsert / (lineWords.size - 1)
        var extraSpaces = spacesToInsert % (lineWords.size - 1)
        lineWords.foldLeft("") { case (acc, w) =>
          if acc == "" then w
          else
            val totalSpaces = if extraSpaces > 0 then
              extraSpaces -= 1
              baseSpaces + 1
            else baseSpaces
            s"$acc${" " * totalSpaces}$w"
        }
    } :+ lastLine.mkString(" ").padTo(maxWidth, ' ')
