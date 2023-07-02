package com.leetcode.cosminci._2800

import scala.util.chaining.*

object _2751_RobotCollisions:

  def survivedRobotsHealths(positions: Array[Int], health: Array[Int], directions: String): List[Int] =
    positions.indices
      .sortBy(positions)
      .foldLeft((Array.empty[Int], health.indices.zip(health).toMap)) { case ((stack, health), i) =>
        if directions(i) == 'R' then (stack :+ i, health)
        else
          Iterator.iterate((stack, health)) { case (stack, health) =>
            val (head, last) = (stack.dropRight(1), stack.last)
            if health(last) < health(i) then (head, health.updated(last, 0).updated(i, health(i) - 1))
            else if health(last) > health(i) then (stack, health.updated(i, 0).updated(last, health(last) - 1))
            else (head, health.updated(last, 0).updated(i, 0))
          }.dropWhile { case (stack, health) => stack.nonEmpty && health(i) > 0 }.next()
      }
      .pipe { case (_, health) => health.toList.sorted.collect { case (i, v) if v > 0 => v } }
