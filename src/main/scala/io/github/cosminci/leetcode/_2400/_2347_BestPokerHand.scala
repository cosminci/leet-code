package io.github.cosminci.leetcode._2400

object _2347_BestPokerHand:

  def bestHand(ranks: Array[Int], suits: Array[Char]): String =
    if suits.min == suits.max then "Flush"
    else
      ranks.groupMapReduce(identity)(_ => 1)(_ + _).values.max match
        case 5 | 4 | 3 => "Three of a Kind"
        case 2         => "Pair"
        case _         => "High Card"
