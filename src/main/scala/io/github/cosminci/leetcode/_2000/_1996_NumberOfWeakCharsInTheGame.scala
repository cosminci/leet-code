package io.github.cosminci.leetcode._2000

object _1996_NumberOfWeakCharsInTheGame:

  def numberOfWeakCharacters(properties: Array[Array[Int]]): Int =
    properties.length - properties
      .sortBy { case Array(attack, defense) => (attack, -defense) }
      .map { case Array(_, defense) => defense }
      .foldLeft(Seq.empty[Int]) { (monoStack, defense) =>
        defense +: Iterator
          .iterate(monoStack)(_.tail)
          .dropWhile(_.headOption.exists(_ < defense))
          .next()
      }
      .length
