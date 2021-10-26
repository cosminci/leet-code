package io.github.cosminci.leetcode._100

import scala.collection.mutable

object _30_SubstringWithContactenationOfAllWords:
  def main(args: Array[String]): Unit =
    println(findSubstring("barfoothefoobarman", Array("foo", "bar")))

  private def findSubstring(s: String, words: Array[String]): List[Int] =
    val wordBag     = words.groupBy(identity).view.mapValues(_.size).toMap
    val wordLength  = words.head.length
    val totalLength = wordLength * words.length

    (0 until s.length - totalLength + 1).foldLeft(List.empty[Int]) { case (acc, left) =>
      val needed = mutable.Map.from(wordBag)

      val matched = (left until left + totalLength by wordLength).forall { right =>
        val word     = s.substring(right, right + wordLength)
        val isNeeded = needed.contains(word)
        needed.updateWith(word) {
          case None | Some(1) => None
          case Some(c)        => Some(c - 1)
        }
        isNeeded
      } && needed.isEmpty

      if matched then acc :+ left else acc
    }
