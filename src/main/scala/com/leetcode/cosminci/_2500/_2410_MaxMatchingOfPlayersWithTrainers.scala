package com.leetcode.cosminci._2500

object _2410_MaxMatchingOfPlayersWithTrainers:

  def matchPlayersAndTrainers(players: Array[Int], trainers: Array[Int]): Int =
    trainers.sortInPlace()
    players.sortInPlace()

    @annotation.tailrec
    def dfs(i: Int, j: Int): Int =
      if i == players.length || j == trainers.length then i
      else if players(i) <= trainers(j) then dfs(i + 1, j + 1)
      else dfs(i, j + 1)

    dfs(i = 0, j = 0)
