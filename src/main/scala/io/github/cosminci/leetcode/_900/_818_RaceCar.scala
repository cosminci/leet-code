package io.github.cosminci.leetcode._900

import scala.collection.mutable

object _818_RaceCar:

  def main(args: Array[String]): Unit =
    println(racecarBFS(5478))

  case class State(position: Int, speed: Int)

  private def racecarBFS(target: Int): Int =
    val initialState = State(0, 1)
    val toVisit      = mutable.Queue(initialState)
    val visited      = mutable.Set(initialState)

    var level = 0
    while toVisit.nonEmpty do
      toVisit.toList.foreach { case State(position, speed) =>
        if position == target then return level

        val accelerateState = State(position + speed, speed * 2)
        val reverseState    = State(position, if speed > 0 then -1 else 1)

        def reasonable(state: State) = state.position < 2 * target && state.position >= 0

        if !visited.contains(reverseState) && reasonable(reverseState) then
          visited.add(reverseState)
          toVisit.enqueue(reverseState)

        if !visited.contains(accelerateState) && reasonable(accelerateState) then
          visited.add(accelerateState)
          toVisit.enqueue(accelerateState)
      }
      level += 1

    -1
    