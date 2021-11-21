package io.github.cosminci.leetcode._2100

object _2079_WateringPlants:
  def wateringPlants(plants: Array[Int], capacity: Int): Int =
    plants.indices.foldLeft(0, capacity) {
      case ((steps, water), idx) =>
        if water < plants(idx) then
          (steps + 2 * idx + 1, capacity - plants(idx))
        else
          (steps + 1, water - plants(idx))
      }._1
