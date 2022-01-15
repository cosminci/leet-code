package io.github.cosminci.leetcode._1300

object _1286_IteratorForCombination:
  def main(args: Array[String]): Unit =
    val it = new CombinationIterator("abcd", 3)
    (1 to 4).foreach { _ =>
      println(it.next())
      println(it.hasNext())
    }

  class CombinationIterator(chars: String, length: Int):
    var curr = ""

    def next(): String =
      if curr.isEmpty then curr = chars.take(length)
      else
        val end = curr.reverse.zip(chars.reverse).takeWhile { case (c1, c2) => c1 == c2 }.length
        val idx = chars.indexOf(curr((length - end - 1) % length))
        curr = curr.take(curr.length - end - 1) + ((chars * 2).slice(idx + 1, idx + 2 + end))
      curr

    def hasNext(): Boolean =
      curr != chars.takeRight(length)
