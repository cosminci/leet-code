package io.github.cosminci.leetcode._800

import scala.collection.mutable

object _767_ReorganizeString:
  def main(args: Array[String]): Unit =
    println(reorganizeString("vvvlo"))

  def reorganizeString(s: String): String =
    val charCounts = s.foldLeft(Map.empty[Char, Int]) { case (counts, char) =>
      counts.updatedWith(char) {
        case None    => Some(1)
        case Some(c) => Some(c + 1)
      }
    }
    given Ordering[(Char, Int)] = (x, y) => x._2.compare(y._2)

    val pqueue = mutable.PriorityQueue.from(charCounts)
    var result = new StringBuilder

    var prev: Option[(Char, Int)] = None
    while pqueue.nonEmpty do
      val (mostFrequentChar, count) = pqueue.dequeue()
      result.append(mostFrequentChar)
      prev.foreach(p => pqueue.enqueue(p))
      prev = Option.when(count > 1)((mostFrequentChar, count - 1))

    if result.length() != s.length then ""
    else result.toString
