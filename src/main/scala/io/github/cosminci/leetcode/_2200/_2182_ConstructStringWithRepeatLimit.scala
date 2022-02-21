package io.github.cosminci.leetcode._2200

import scala.collection.mutable
import scala.collection.mutable.TreeMap

object _2182_ConstructStringWithRepeatLimit:

  def repeatLimitedString(s: String, repeatLimit: Int): String =
    val result = new StringBuilder("_")
    val counts = mutable.TreeMap.from(s.groupMapReduce(identity)(_ => 1)(_ + _))(Ordering.Char.reverse)

    @annotation.tailrec
    def dfs(): Unit =
      if counts.isEmpty then ()
      else
        val maybeHead = Option.when(counts.head._1 != result.last)(counts.head)
        maybeHead.orElse(counts.minAfter((result.last - 1).toChar)) match
          case None => ()
          case Some((char, count)) =>
            val repeat = if counts.head._1 != char then 1 else count.min(repeatLimit)
            result.append(s"$char" * repeat)
            updateCount(counts, char, repeat)
            dfs()

    def updateCount(counts: mutable.TreeMap[Char, Int], char: Char, repeat: Int): Unit =
      counts.updateWith(char) {
        case None                   => None
        case Some(v) if v <= repeat => None
        case Some(v)                => Some(v - repeat)
      }

    dfs()
    result.drop(1).toString()
