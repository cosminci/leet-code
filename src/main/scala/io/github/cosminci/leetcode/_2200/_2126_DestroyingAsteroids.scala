package io.github.cosminci.leetcode._2200

import scala.collection.immutable.TreeSet

object _2126_DestroyingAsteroids:

  def asteroidsDestroyed(mass: Int, asteroids: Array[Int]): Boolean =
    @annotation.tailrec
    def dfs(mass: Int, remaining: TreeSet[(Int, Int)]): Boolean =
      if remaining.lastOption.forall(_._1 <= mass) then true
      else remaining.maxBefore((mass, Int.MaxValue)) match
        case None              => false
        case Some(destroyable) => dfs(mass + destroyable._1, remaining - destroyable)

    dfs(mass, TreeSet.from(asteroids.zipWithIndex))
