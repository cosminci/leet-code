package com.leetcode.cosminci._1000

import scala.collection.mutable

object _904_FruitIntoBaskets:

  def totalFruit(fruits: Array[Int]): Int = {
    val mem = mutable.Map.empty[(Int, Set[Int]), Int]

    def dfs(i: Int, fruitTypes: Set[Int]): Int = mem.getOrElseUpdate((i, fruitTypes),
      if (i == fruits.length) 0
      else if (fruitTypes.size == 2 && !fruitTypes.contains(fruits(i))) 0
      else 1 + dfs(i + 1, fruitTypes + fruits(i))
    )

    fruits.indices.map(dfs(_, Set.empty)).max
  }
