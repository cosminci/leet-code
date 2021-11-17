package io.github.cosminci.leetcode._2100

import scala.collection.mutable

object _2073_TimeNeededToBuyTickets {
  def main(args: Array[String]): Unit =
    println(timeRequiredToBuy(Array(2, 3, 2), 2))

  def timeRequiredToBuy(tickets: Array[Int], k: Int): Int =
    tickets.indices.foldLeft(0) { (acc, i) =>
      acc + tickets(i).min(tickets(k) - Option.when(i <= k)(0).getOrElse(1))
    }
}
