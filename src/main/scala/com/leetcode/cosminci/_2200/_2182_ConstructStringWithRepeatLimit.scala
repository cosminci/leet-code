package com.leetcode.cosminci._2200

import scala.collection.immutable.TreeMap
import scala.collection.mutable

object _2182_ConstructStringWithRepeatLimit {
  def main(args: Array[String]): Unit = {
    println(repeatLimitedString("cczazcc", 3))
  }

  def repeatLimitedString(s: String, repeatLimit: Int): String = {
    @annotation.tailrec
    def dfs(counts: TreeMap[Char, Int], result: StringBuilder, prev: Char): String =
      if (counts.isEmpty) result.toString()
      else {
        Option.when(counts.head._1 != prev)(counts.head).orElse(counts.minAfter((prev - 1).toChar)) match {
          case None => result.toString()
          case Some((char, count)) =>
            val repeat = if (counts.head._1 != char) 1 else count.min(repeatLimit)
            dfs(subtract(counts, char, repeat), result.append(char.toString * repeat), char)
        }
      }

    def subtract(counts: TreeMap[Char, Int], char: Char, repeat: Int): TreeMap[Char, Int] =
      counts.updatedWith(char) {
        case None => None
        case Some(v) if v <= repeat => None
        case Some(v) => Some(v - repeat)
      }

    val counts = TreeMap.from(s.groupMapReduce(identity)(_ => 1)(_ + _))(Ordering.Char.reverse)
    dfs(counts, result = new mutable.StringBuilder, prev = '_')
  }
}
