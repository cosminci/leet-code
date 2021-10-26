package io.github.cosminci.leetcode._2000

import scala.collection.mutable

object _1921_EliminateMaxNumberOfMonsters:

  def main(args: Array[String]): Unit =
    println(eliminateMaximum(Array(1, 3, 4, 2, 3), Array(1, 1, 1, 1, 1)))

  private def eliminateMaximum(dist: Array[Int], speed: Array[Int]): Int =
    def computeMinutesLeft(monster: Int) = math.ceil(dist(monster).toDouble / speed(monster)).toInt

    val minutesLeft = Array.ofDim[Int](dist.length)
    dist.indices.foreach { monster =>
      minutesLeft(monster) = computeMinutesLeft(monster)
    }

    given Ordering[Int] = (x: Int, y: Int) => y.compare(x)
    val arrivalQueue    = mutable.PriorityQueue.from(minutesLeft)

    var currentTime = 0
    var killed      = 0
    while arrivalQueue.nonEmpty do
      val minsLeft = arrivalQueue.dequeue()
      if minsLeft <= currentTime then return killed
      else killed += 1
      currentTime += 1

    killed
