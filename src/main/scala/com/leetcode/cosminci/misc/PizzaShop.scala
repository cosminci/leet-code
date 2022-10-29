package com.leetcode.cosminci.misc

object PizzaShop:

  def main(args: Array[String]): Unit =
    println(closestPrice(Array(800, 850, 900), Array(100, 150), 1000))
    println(closestPrice(Array(850, 900), Array(200, 250), 1000))
    println(closestPrice(Array(1100, 900), Array(200), 1000))
    println(closestPrice(Array(800, 800, 800, 800), Array(100), 1000))

  def closestPrice(pizzas: Array[Int], toppings: Array[Int], budget: Int) =
    val toppingMixes = toppings.indices.flatMap(i => (i + 1 until toppings.length).map(j => toppings(i) + toppings(j)))
    val toppingOptions = new java.util.TreeSet[Int]()
    toppingMixes.appendedAll(toppings).appended(0).foreach(toppingOptions.add)

    pizzas
      .flatMap { pizza =>
        val closestHigher = toppingOptions.ceiling(budget - pizza) + pizza
        val closestLower  = toppingOptions.floor(budget - pizza) + pizza
        if math.abs(budget - closestHigher) >= math.abs(budget - closestLower) then List(closestLower)
        else List(closestHigher)
      }
      .foldLeft(Int.MaxValue) { case (closest, cost) =>
        if math.abs(budget - cost) < math.abs(budget - closest) then cost
        else if math.abs(budget - cost) == math.abs(budget - closest) && cost < closest then cost
        else closest
      }
