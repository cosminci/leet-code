package io.github.cosminci.leetcode._1000

import scala.collection.mutable

object _904_FruitIntoBaskets:

  def main(args: Array[String]): Unit =
    println(totalFruitTopDown(Array(3, 3, 3, 1, 2, 1, 1, 2, 3, 3, 4)))
    println(totalFruitSliding(Array(3, 3, 3, 1, 2, 1, 1, 2, 3, 3, 4)))

  private def totalFruitSliding(fruits: Array[Int]): Int =
    val baskets = mutable.Map.empty[Int, Int]
    fruits(2) = 3
    var max    = 0
    var (l, r) = (0, 0)
    while r < fruits.length do
      if baskets.size == 2 && !baskets.contains(fruits(r)) then
        max = math.max(max, baskets.values.sum)
        while baskets.size == 2 do
          baskets.updateWith(fruits(l)) {
            case Some(1) | None => None
            case Some(c)        => Some(c - 1)
          }
          l += 1
      baskets.updateWith(fruits(r)) {
        case None    => Some(1)
        case Some(c) => Some(c + 1)
      }
      r += 1
    math.max(max, baskets.values.sum)

  private def totalFruitTopDown(fruits: Array[Int]): Int =
    val mem = mutable.Map.empty[(Int, Set[Int]), Int]

    def dfs(idx: Int, fruitTypes: Set[Int]): Int =
      if idx == fruits.length then return 0
      if fruitTypes.size == 2 && !fruitTypes.contains(fruits(idx)) then return 0
      if mem.contains((idx, fruitTypes)) then return mem((idx, fruitTypes))

      val result = 1 + dfs(idx + 1, fruitTypes + fruits(idx))
      mem.update((idx, fruitTypes), result)

      result

    fruits.indices.map(start => dfs(start, Set.empty)).max
