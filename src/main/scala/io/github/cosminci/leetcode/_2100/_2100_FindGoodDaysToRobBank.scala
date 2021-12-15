package io.github.cosminci.leetcode._2100

object _2100_FindGoodDaysToRobBank:
  def main(args: Array[String]): Unit =
    println(goodDaysToRobBank(Array(5, 3, 3, 3, 5, 6, 2), 2))

  def goodDaysToRobBank(security: Array[Int], time: Int): List[Int] =
    val nonIncreasing = (1 until security.length).scanLeft(0) { (count, day) =>
      Option.when(security(day) > security(day - 1))(0).getOrElse(count + 1)
    }
    val nonDecreasing = (0 until security.length - 1).scanRight(0) { (day, count) =>
      Option.when(security(day) > security(day + 1))(0).getOrElse(count + 1)
    }
    security.indices.filter(day => nonIncreasing(day) >= time && nonDecreasing(day) >= time).toList
