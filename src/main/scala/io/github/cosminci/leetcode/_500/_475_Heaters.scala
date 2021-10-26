package io.github.cosminci.leetcode._500

object _475_Heaters:
  def main(args: Array[String]): Unit =
    println(findRadius(Array(1, 2, 3, 4), Array(1, 4)))
    println(findRadius(Array(1, 5), Array(2)))
    println(findRadius(Array(1, 5), Array(10)))

  def findRadius(houses: Array[Int], heaters: Array[Int]): Int =
    val sortedHeaters = new java.util.TreeSet[Int]()
    heaters.forall(sortedHeaters.add)

    houses.map { house =>
      (Option(sortedHeaters.floor(house)), Option(sortedHeaters.ceiling(house))) match
        case (Some(prevClosest), Some(nextClosest)) =>
          math.min(house - prevClosest, nextClosest - house)
        case (Some(prevClosest), None) =>
          house - prevClosest
        case (None, Some(nextClosest)) =>
          nextClosest - house
        case _ => 0
    }.max
