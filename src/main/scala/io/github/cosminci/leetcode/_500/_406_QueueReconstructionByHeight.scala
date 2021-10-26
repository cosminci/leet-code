package io.github.cosminci.leetcode._500

import java.util.LinkedList

object _406_QueueReconstructionByHeight:
  def main(args: Array[String]): Unit =
    println(
      reconstructQueue(
        Array(Array(7, 0), Array(4, 4), Array(7, 1), Array(5, 0), Array(6, 1), Array(5, 2))
      ).map(_.toList).toList
    )

  private def reconstructQueue(people: Array[Array[Int]]): Array[Array[Int]] =
    val result = new LinkedList[Array[Int]]()
    people
      .sortWith { case (x, y) =>
        if x(0) == y(0) then x(1) < y(1) else y(0) < x(0)
      }
      .foreach { person =>
        result.add(person(1), person)
      }
    return result.toArray(people)
