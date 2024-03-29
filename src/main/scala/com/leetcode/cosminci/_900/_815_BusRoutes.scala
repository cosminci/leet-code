package com.leetcode.cosminci._900

object _815_BusRoutes:

  def numBusesToDestination(routes: Array[Array[Int]], source: Int, target: Int): Int =
    val stopBoard = routes.zipWithIndex.foldLeft(Map.empty[Int, Seq[Int]]) { case (acc, (stops, bus)) =>
      stops.foldLeft(acc) { case (acc, stop) =>
        acc.updated(stop, acc.getOrElse(stop, Seq.empty) :+ bus)
      }
    }

    @annotation.tailrec
    def dfs(possibleStops: Seq[Int], usedBuses: Set[Int], numBuses: Int): Int =
      if possibleStops.isEmpty then -1
      else
        val (nextPossibleStops, nextUsedBuses) =
          possibleStops.foldLeft(Seq.empty[Int], usedBuses) { case ((nextStops, usedBuses), currStop) =>
            stopBoard(currStop)
              .filterNot(usedBuses.contains)
              .foldLeft((nextStops, usedBuses)) { case ((nextStops, usedBuses), bus) =>
                if routes(bus).contains(target) then return numBuses + 1
                (nextStops ++ routes(bus), usedBuses + bus)
              }
          }
        dfs(nextPossibleStops, nextUsedBuses, numBuses + 1)

    if source == target then 0
    else dfs(possibleStops = Seq(source), usedBuses = Set.empty, numBuses = 0)
