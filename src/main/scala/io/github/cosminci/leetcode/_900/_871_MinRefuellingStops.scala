package io.github.cosminci.leetcode._900

import scala.collection.immutable.TreeSet

object _871_MinRefuellingStops:

  def minRefuelStops(target: Int, startFuel: Int, stations: Array[Array[Int]]): Int = {
    (stations :+ Array(target, 0)).foldLeft(startFuel, 0, TreeSet.empty[(Int, Int)]) {
      case ((reachable, stops, pastStations), Array(position, fuel)) =>
        if (reachable >= target) return stops

        val (newReachable, newStops, newPastStations) = Iterator.iterate((reachable, stops, pastStations)) {
          case (reachable, stops, pastStations) =>
            pastStations.lastOption match {
              case None => return -1
              case Some(st @ (fuelToAdd, _)) => (reachable + fuelToAdd, stops + 1, pastStations - st)
            }
        }.dropWhile { case (reachable, _, _) => reachable < position }.next()

        (newReachable, newStops, newPastStations + (fuel -> position))
    }._2
  }
