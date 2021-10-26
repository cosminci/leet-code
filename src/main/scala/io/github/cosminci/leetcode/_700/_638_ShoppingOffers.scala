package io.github.cosminci.leetcode._700

import scala.collection.mutable

object _638_ShoppingOffers:
  def main(args: Array[String]): Unit =
    println(shoppingOffers(List(2, 5), List(List(3, 0, 5), List(1, 2, 10)), List(3, 2)))
    println(shoppingOffers(List(2, 3, 4), List(List(1, 1, 0, 4), List(2, 2, 1, 9)), List(1, 2, 1)))
    println(shoppingOffers(List(0, 0, 0), List(List(1, 1, 0, 4), List(2, 2, 1, 9)), List(1, 1, 1)))

  private def shoppingOffers(price: List[Int], special: List[List[Int]], totalNeeds: List[Int]): Int =
    val mem = mutable.Map.empty[Seq[Int], Int]

    def dfs(needs: Seq[Int]): Int =
      mem.getOrElseUpdate(
        needs, {
          if needs.forall(_ == 0) then 0
          else {
            special.collect {
              case offer if needs.indices.forall(i => offer(i) <= needs(i)) =>
                offer.last + dfs(needs.indices.map(i => needs(i) - offer(i)))
            } :+ needs.indices.foldLeft(0)((sum, i) => sum + needs(i) * price(i))
          }.min
        }
      )

    dfs(totalNeeds.toSeq)
