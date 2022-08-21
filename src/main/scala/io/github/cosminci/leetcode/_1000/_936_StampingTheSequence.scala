package io.github.cosminci.leetcode._1000

object _936_StampingTheSequence:

  def movesToStamp(stamp: String, target: String): Array[Int] =
    def canRemoveStamp(target: Map[Int, Char], start: Int): Boolean =
      stamp.indices.foldLeft(false) { (hasMatchingLetter, i) =>
        if target(start + i) == '?' then hasMatchingLetter
        else if target(start + i) == stamp(i) then true
        else return false
      }

    @annotation.tailrec
    def movesToStamp(target: Map[Int, Char], stampings: List[Int]): List[Int] =
      val (newTarget, removedStamp, newStampings) = (0 to target.size - stamp.length)
        .foldLeft(target, false, stampings) { case ((target, removedStamp, stampings), start) =>
          if !canRemoveStamp(target, start) then (target, removedStamp, stampings)
          else (target ++ (start until start + stamp.length).map(_ -> '?'), true, start +: stampings)
        }
      if removedStamp then movesToStamp(newTarget, newStampings)
      else if newTarget.values.forall(_ == '?') then newStampings
      else List.empty

    movesToStamp(target.indices.zip(target).toMap, stampings = List.empty).toArray
