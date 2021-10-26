package io.github.cosminci.leetcode._1400

import scala.collection.mutable

object _1306_JumpGameIII:

  private def canReach(arr: Array[Int], start: Int): Boolean =
    val toVisit = mutable.Stack(start)
    val visited = mutable.Set(start)

    while toVisit.nonEmpty do
      val position = toVisit.pop()
      if arr(position) == 0 then return true
      val frontJump = position + arr(position)
      if frontJump < arr.length && !visited.contains(frontJump) then
        toVisit.push(frontJump)
        visited.add(frontJump)
      val backJump = position - arr(position)
      if backJump >= 0 && !visited.contains(backJump) then
        toVisit.push(backJump)
        visited.add(backJump)

    false
