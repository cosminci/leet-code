package io.github.cosminci.leetcode._900

object _890_FindAndReplacePattern:
  def main(args: Array[String]): Unit =
    println(findAndReplacePattern(Array("abc", "deq", "mee", "aqq", "dkd", "ccc"), "abb"))

  def findAndReplacePattern(words: Array[String], pattern: String): List[String] =
    def normalize(s: String) =
      s.foldLeft(Seq.empty[Int], Map.empty[Char, Int]) { case ((pattern, mappings), char) =>
        mappings.get(char) match
          case Some(prevCode) =>
            (pattern :+ prevCode, mappings)
          case None =>
            val newCode = mappings.size
            (pattern :+ newCode, mappings.updated(char, newCode))
      }._1

    words.filter(w => normalize(w) == normalize(pattern)).toList
