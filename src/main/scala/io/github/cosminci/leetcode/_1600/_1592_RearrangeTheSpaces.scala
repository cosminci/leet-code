package io.github.cosminci.leetcode._1600

object _1592_RearrangeTheSpaces:

  def main(args: Array[String]): Unit =
    println(reorderSpaces("  this   is  a sentence "))

  private def reorderSpaces(text: String): String =
    val spaces = text.count(_ == ' ')
    val words = text.split(" ").collect {
      case w if w.trim.nonEmpty =>
        w.trim
    }
    if words.length == 0 then return " " * spaces
    if words.length == 1 then return words.head + " " * spaces

    val spacesBetweenWords = " " * (spaces / (words.length - 1))
    val spacesAtEnd        = " " * (spaces % (words.length - 1))
    s"${words.mkString(spacesBetweenWords)}$spacesAtEnd"
