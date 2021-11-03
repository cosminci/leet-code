package io.github.cosminci.leetcode._800

object _789_EscapeTheGhosts:
  def escapeGhosts(ghosts: Array[Array[Int]], target: Array[Int]): Boolean =
    val distanceToTarget = math.abs(target(0)) + math.abs(target(1))
    ghosts.forall(ghost => math.abs(ghost(0) - target(0)) + math.abs(ghost(1) - target(1)) > distanceToTarget)
