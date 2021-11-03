package io.github.cosminci.leetcode._1100

object _1007_MinDominoRotationsForEqualRow:
  def main(args: Array[String]): Unit =
    println(minDominoRotations(Array(1, 1, 1, 1, 2, 1), Array(2, 1, 1, 1, 2, 1)))
    println(minDominoRotations2(Array(2, 1, 2, 4, 2, 2), Array(5, 2, 6, 2, 3, 2)))

  def minDominoRotations(tops: Array[Int], bottoms: Array[Int]): Int =
    if tops.length == 1 then return 0

    val dominoes = tops.zip(bottoms)
    dominoes.tail
      .foldLeft(Set(tops.head, bottoms.head)) { case (candidates, (top, bottom)) =>
        if !candidates.contains(top) && !candidates.contains(bottom) then return -1
        else if candidates.contains(top) && candidates.contains(bottom) then Set(top, bottom)
        else if candidates.contains(top) then Set(top)
        else Set(bottom)
      }
      .map { candidate =>
        val (totalHeadSwitches, totalTailSwitches) = dominoes.foldLeft((0, 0)) {
          case ((headSwitches, tailSwitches), (top, bottom)) =>
            if top != candidate then (headSwitches + 1, tailSwitches)
            else if bottom != candidate then (headSwitches, tailSwitches + 1)
            else (headSwitches, tailSwitches)
        }
        math.min(totalHeadSwitches, totalTailSwitches)
      }
      .min

  def minDominoRotations2(tops: Array[Int], bottoms: Array[Int]): Int =
    if tops.length == 1 then return 0

    val dominoes              = tops.zip(bottoms)
    val (headTop, headBottom) = dominoes.head

    def countSwitches(value: Int) =
      val (totalTopSwitches, totalBottomSwitches) = dominoes.foldLeft((0, 0)) {
        case (acc @ (topSwitches, bottomSwitches), (top, bottom)) =>
          if acc == (-1, -1) then acc
          else if top != value && bottom != value then (-1, -1)
          else if top == value && bottom == value then acc
          else if top == value then (topSwitches, bottomSwitches + 1)
          else (topSwitches + 1, bottomSwitches)
      }
      math.min(totalTopSwitches, totalBottomSwitches)

    val topSwitches    = countSwitches(headTop)
    val bottomSwitches = countSwitches(headBottom)

    if topSwitches == -1 then bottomSwitches
    else if bottomSwitches == -1 then topSwitches
    else math.min(topSwitches, bottomSwitches)
