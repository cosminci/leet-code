package com.leetcode.cosminci._900

object _881_BoatsToSavePeople:
  def main(args: Array[String]): Unit =
    println(numRescueBoats(Array(3, 2, 2, 1), 3))

  def numRescueBoats(people: Array[Int], limit: Int): Int =
    people.sortInPlace()

    @annotation.tailrec
    def dfs(l: Int, r: Int, boats: Int): Int =
      if l > r then boats
      else if people(l) + people(r) <= limit then dfs(l + 1, r - 1, boats + 1)
      else dfs(l, r - 1, boats + 1)

    dfs(l = 0, r = people.length - 1, boats = 0)
