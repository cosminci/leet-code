package io.github.cosminci.leetcode._2000

import scala.collection.mutable

object _1996_NumberOfWeakCharsInTheGame {
  def main(args: Array[String]): Unit = {
    println(numberOfWeakCharacters(Array(Array(5, 5), Array(6, 3), Array(3, 6))))
    println(numberOfWeakCharacters(Array(Array(2, 2), Array(2, 3))))
    println(numberOfWeakCharacters(Array(Array(1, 5), Array(10, 4), Array(4, 3))))
  }

  private def numberOfWeakCharacters(properties: Array[Array[Int]]): Int = {
    given Ordering[Array[Int]] = (x, y) => if (x.head == y.head) y.last.compare(x.last) else x.head.compare(y.head)
    properties.sortInPlace()

    val higherDefense = mutable.Stack.empty[Int]
    var count = 0
    properties.foreach { case Array(attack, defense) =>
      while (higherDefense.headOption.exists(_ < defense)) {
        higherDefense.pop()
        count += 1
      }
      higherDefense.push(defense)
    }
    count
  }
}
