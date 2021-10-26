package io.github.cosminci.leetcode._100

import scala.collection.mutable

object _22_GenerateParantheses:
  def main(args: Array[String]): Unit =
    println(generateParenthesis(4))

  def generateParenthesis(n: Int): List[String] =
    if n == 0 then return List.empty

    def dfs(n: Int): Set[String] =
      if n == 1 then return Set("()")

      dfs(n - 1).flatMap { prevValid =>
        val basis = new StringBuilder(prevValid)
        (0 to basis.length).map { i =>
          val newValid = basis.insert(i, "()").toString
          basis.delete(i, i + 2)
          newValid
        }
      }

    dfs(n).toList
