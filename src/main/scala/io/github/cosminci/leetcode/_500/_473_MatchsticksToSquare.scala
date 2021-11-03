package io.github.cosminci.leetcode._500

object _473_MatchsticksToSquare:
  def main(args: Array[String]): Unit =
    println(makesquare(Array(1, 1, 2, 2, 2)))
    println(makesquare(Array(3, 3, 3, 3, 4)))

  def makesquare(matchsticks: Array[Int]): Boolean =
    matchsticks.sortBy(-_)
    val totalLength = matchsticks.sum
    val sideLength  = totalLength / 4
    if totalLength % 4 != 0 || matchsticks.exists(_ > sideLength) then return false

    def dfs(sides: Seq[Int], idx: Int): Boolean =
      if idx == matchsticks.length then return true

      val matchstick = matchsticks(idx)
      sides.indices.exists { i =>
        val newSideLength = sides(i) + matchstick
        newSideLength <= sideLength && dfs(sides.updated(i, newSideLength), idx + 1)
      }

    dfs(Seq.fill(4)(0), 0)
