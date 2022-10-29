package com.leetcode.cosminci._900

import scala.collection.mutable

object _846_HandOfStraights:
  def main(args: Array[String]): Unit =
    println(isNStraightHand(Array(1, 2, 3, 6, 2, 3, 4, 7, 8), 3))
    println(isNStraightHand(Array(1, 2, 3, 7, 2, 3, 4, 7, 8), 3))

  def isNStraightHand(hand: Array[Int], groupSize: Int): Boolean =
    if hand.length % groupSize != 0 then return false

    val counts = mutable.TreeMap.empty[Int, Int]
    hand.foreach { card =>
      counts.updateWith(card) {
        case None    => Some(1)
        case Some(c) => Some(c + 1)
      }
    }

    while counts.keysIterator.nonEmpty do
      val card = counts.keysIterator.next()
      removeCard(counts, card)
      (1 until groupSize).foreach { i =>
        if !counts.contains(card + i) then return false
        else removeCard(counts, card + i)
      }
    true

  def removeCard(counts: mutable.TreeMap[Int, Int], card: Int): Unit =
    counts.updateWith(card) {
      case None | Some(1) => None
      case Some(c)        => Some(c - 1)
    }
