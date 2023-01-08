package com.leetcode.cosminci._2600

object _2531_MaxNumDistinctCharsEqual:

  def isItPossible(word1: String, word2: String): Boolean =
    val counts1 = word1.groupMapReduce(identity)(_ => 1)(_ + _).withDefaultValue(0)
    val counts2 = word2.groupMapReduce(identity)(_ => 1)(_ + _).withDefaultValue(0)

    counts1.keySet.exists { char1 =>
      counts2.keySet.exists { char2 =>
        if char1 == char2 then counts1.size == counts2.size
        else update(counts1, char2, char1).size == update(counts2, char1, char2).size
      }
    }

  private def update(counts: Map[Char, Int], toAdd: Char, toRemove: Char) =
    counts
      .updated(toAdd, counts(toAdd) + 1)
      .updatedWith(toRemove) {
        case Some(cnt) if cnt >= 2 => Some(cnt - 1)
        case _                     => None
      }
