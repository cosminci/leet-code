package io.github.cosminci.leetcode._2300

object _2225_FindPlayersWithZeroOrOneLosses:
  def findWinners(matches: Array[Array[Int]]): List[List[Int]] =
    val (zeroLosses, singleLosses) = matches
      .foldLeft(Map.empty[Int, Int].withDefaultValue(0)) { case (losses, Array(winner, loser)) =>
        losses.updated(winner, losses(winner)).updated(loser, losses(loser) + 1)
      }
      .foldLeft(Array.empty[Int], Array.empty[Int]) { case ((zeroLosses, singleLosses), (player, lossCount)) =>
        if lossCount == 0 then (zeroLosses :+ player, singleLosses)
        else if lossCount == 1 then (zeroLosses, singleLosses :+ player)
        else (zeroLosses, singleLosses)
      }

    List(zeroLosses.sorted.toList, singleLosses.sorted.toList)
