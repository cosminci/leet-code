package io.github.cosminci.leetcode._900

object _838_PushDominoes:
  def main(args: Array[String]): Unit =
    println(pushDominoes(".L.R...LR..L.."))

  private def pushDominoes(dominoes: String): String =
    val closestRight = Array.ofDim[Int](dominoes.length)
    dominoes.indices.foreach { i =>
      closestRight(i) =
        if dominoes(i) == 'R' then 0
        else if i > 0 && closestRight(i - 1) != Int.MaxValue && dominoes(i) == '.' then closestRight(i - 1) + 1
        else Int.MaxValue
    }

    val closestLeft = Array.ofDim[Int](dominoes.length)
    (dominoes.length - 1 to 0 by -1).foreach { i =>
      closestLeft(i) =
        if dominoes(i) == 'L' then 0
        else if i < dominoes.length - 1 && closestLeft(i + 1) != Int.MaxValue && dominoes(i) == '.' then
          closestLeft(i + 1) + 1
        else Int.MaxValue
    }

    val result = Array.ofDim[Char](dominoes.length)
    dominoes.indices.foreach { i =>
      result(i) =
        if closestLeft(i) < closestRight(i) then 'L'
        else if closestLeft(i) > closestRight(i) then 'R'
        else '.'
    }
    result.mkString
