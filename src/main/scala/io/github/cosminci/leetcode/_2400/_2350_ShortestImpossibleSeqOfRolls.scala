package io.github.cosminci.leetcode._2400

object _2350_ShortestImpossibleSeqOfRolls:

  def shortestSequence(rolls: Array[Int], k: Int): Int =
    rolls.foldLeft(1, Set.empty[Int]) {
      case ((shortest, seen), n) =>
        if (seen + n).size == k then (shortest + 1, Set.empty)
        else (shortest, seen + n)
      }._1
