package io.github.cosminci.leetcode._2200

import scala.collection.mutable

object _2115_FindAllPossibleRecipesFromGivenSupplies:
  def main(args: Array[String]): Unit =
    println(
      findAllRecipes(
        Array("bread", "sandwich"),
        List(List("yeast", "flour"), List("bread", "meat")),
        Array("yeast", "flour", "meat")
      )
    )

  def findAllRecipes(recipes: Array[String], ingredients: List[List[String]], supplies: Array[String]): List[String] =
    val available  = supplies.toSet
    val recipeBook = recipes.zip(ingredients).toMap

    def canCreate(recipe: String, reqs: Set[String]): Boolean =
      recipeBook.get(recipe).exists(_.forall { ingredient =>
        !reqs.contains(ingredient) &&
          (available.contains(ingredient) || canCreate(ingredient, reqs + ingredient))
      })

    recipes.filter(canCreate(_, Set.empty)).toList
