package io.github.cosminci.leetcode._1900

import scala.collection.mutable

object _1871_JumpGameVII:
  def main(args: Array[String]): Unit =
    println(canReach("011001110001000", 3, 5))
    println(canReach("0000000000", 2, 5))
    println(canReach("0000000000", 8, 8))
    println(canReach("011010", 2, 3))

  private def canReach(s: String, minJump: Int, maxJump: Int): Boolean =
    if s.head == '1' || s.last == '1' then return false
    var farthest   = 0
    val jumpPoints = mutable.Queue(0)
    while jumpPoints.nonEmpty do
      val start        = jumpPoints.dequeue()
      val minJumpIndex = math.max(start + minJump, farthest + 1)
      val maxJumpIndex = math.min(start + maxJump, s.length - 1)
      (minJumpIndex to maxJumpIndex).foreach { jump =>
        if jump == s.length - 1 then return true
        if s(jump) == '0' then jumpPoints.enqueue(jump)
        farthest = maxJumpIndex
      }
    false
