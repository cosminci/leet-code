package com.leetcode.cosminci._2300

object _2260_MinConsecutiveCardsToPickUp:

  def minimumCardPickup(cards: Array[Int]): Int =
    val minDistance = cards.zipWithIndex
      .foldLeft(Map.empty[Int, Int], Int.MaxValue) { case ((lastPosition, minDistance), (card, idx)) =>
        lastPosition.get(card) match
          case Some(prevIdx) => (lastPosition.updated(card, idx), minDistance.min(idx - prevIdx + 1))
          case None          => (lastPosition.updated(card, idx), minDistance)
      }._2
    if minDistance == Int.MaxValue then -1 else minDistance
