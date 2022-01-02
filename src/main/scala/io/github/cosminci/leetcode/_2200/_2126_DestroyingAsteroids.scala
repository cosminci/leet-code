package io.github.cosminci.leetcode._2200

import scala.collection.immutable.TreeSet

object _2126_DestroyingAsteroids:

  def asteroidsDestroyed(mass: Int, asteroids: Array[Int]): Boolean =
    asteroids.sorted.foldLeft(mass.toLong) { (mass, asteroid) =>
      if asteroid > mass then return false else mass + asteroid
    } > 0
