package io.github.cosminci.leetcode._900

object _881_BoatsToSavePeople:
  def main(args: Array[String]): Unit =
    println(numRescueBoats(Array(3, 2, 2, 1), 3))

  def numRescueBoats(people: Array[Int], limit: Int): Int =
    people.sortInPlace()
    var (l, r)   = (0, people.length - 1)
    var numBoats = 0

    while l <= r do
      if people(l) + people(r) <= limit then l += 1
      r -= 1
      numBoats += 1

    numBoats
