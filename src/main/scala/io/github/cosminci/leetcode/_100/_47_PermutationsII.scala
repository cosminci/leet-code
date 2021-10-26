package io.github.cosminci.leetcode._100

import scala.collection.mutable

object _47_PermutationsII:
  def main(args: Array[String]): Unit =
    println(permuteUnique(Array(1, 2, 3)))

  private def permuteUnique(input: Array[Int]): List[List[Int]] =
    val mem = mutable.Map.empty[Int, Set[Seq[Int]]]

    def dfs(idx: Int): Set[Seq[Int]] =
      if idx == input.length - 1 then return Set(Seq(input.last))
      if mem.contains(idx) then return mem(idx)

      val result = dfs(idx + 1).flatMap { permutation =>
        (0 to permutation.size).map { i =>
          (permutation.take(i) :+ input(idx)) ++ permutation.takeRight(permutation.size - i)
        }
      }
      mem.update(idx, result)
      result

    dfs(0).map(_.toList).toList
