package com.leetcode.cosminci._2000

import scala.collection.mutable

object _1976_NumberOfWaysToArriveAtDestination:
  def main(args: Array[String]): Unit =
    println(
      countPaths(
        5,
        Array(
          Array(0, 1, 1000000000),
          Array(1, 2, 1000000000),
          Array(2, 3, 1000000000),
          Array(3, 4, 1000000000)
        )
      )
    )

  case class Ways(count: Long, cost: Long)
  case class Path(from: Int, to: Int, cost: Long)

  def countPaths(n: Int, roads: Array[Array[Int]]): Int =
    val mod          = 1_000_000_007
    val waysToArrive = Array.fill[Ways](n)(Ways(0, Long.MaxValue))

    val adjMatrix = roads.foldLeft(Map.empty[Int, Seq[Path]].withDefaultValue(Seq.empty)) {
      case (acc, Array(from, to, cost)) =>
        acc
          .updated(from, acc(from).appended(Path(from, to, cost)))
          .updated(to, acc(to).appended(Path(to, from, cost)))
    }
    waysToArrive(0) = Ways(1, Long.MaxValue)

    val toVisit =
      given Ordering[Path] = (x, y) => y.cost.compare(x.cost)
      mutable.PriorityQueue(Path(0, 0, 0))

    while toVisit.nonEmpty do
      val Path(before, curr, cost) = toVisit.dequeue()
      val prevWays                 = waysToArrive(curr)
      if cost == prevWays.cost then
        waysToArrive(curr) = prevWays.copy(count = (prevWays.count + waysToArrive(before).count) % mod)
      else if cost < prevWays.cost then
        waysToArrive(curr) = Ways(waysToArrive(before).count, cost)
        adjMatrix(curr).foreach { case Path(from, to, newCost) =>
          if to != before then toVisit.enqueue(Path(curr, to, cost + newCost))
        }

    waysToArrive.last.count.toInt
