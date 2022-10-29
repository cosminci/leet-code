package com.leetcode.cosminci._2400

import scala.collection.mutable

object _2353_DesignAGoodRatingSystem:

  class FoodRatings(foods: Array[String], cuisines: Array[String], ratings: Array[Int]):

    private val foodToCuisine = foods.zip(cuisines).toMap
    private val foodRatings   = mutable.Map.from(foods.zip(ratings))
    private val cuisineToFoods = mutable.Map.from(
      foods.indices
        .groupBy(cuisines)
        .view
        .mapValues { indices =>
          val ratingNamePairs = indices.map(i => (-ratings(i), foods(i)))
          mutable.TreeSet.from(ratingNamePairs)
        }
    )

    def changeRating(food: String, newRating: Int): Boolean =
      val oldRating = foodRatings(food)
      foodRatings.update(food, newRating)
      cuisineToFoods(foodToCuisine(food)).remove((-oldRating, food))
      cuisineToFoods(foodToCuisine(food)).add((-newRating, food))

    def highestRated(cuisine: String): String =
      cuisineToFoods(cuisine).head._2
