package com.leetcode.cosminci._700

import scala.collection.mutable

object _691_StickersToSpellWord:
  def main(args: Array[String]): Unit =
    println(minStickers(Array("with", "example", "science"), "thehat"))
    println(minStickers(Array("notice", "possible"), "placeclock"))
    println(
      minStickers(
        Array(
          "among",
          "just",
          "people",
          "ran",
          "sound",
          "son",
          "wash",
          "design",
          "mark",
          "dress",
          "arm",
          "lie",
          "little",
          "copy",
          "develop",
          "beauty",
          "support",
          "sky",
          "tail",
          "should",
          "machine",
          "few",
          "written",
          "board",
          "told",
          "flat",
          "parent",
          "though",
          "material",
          "chart",
          "hand",
          "wear",
          "fresh",
          "teach",
          "general",
          "wont",
          "word",
          "third",
          "light",
          "region",
          "hunt",
          "cover",
          "together",
          "crease",
          "sand",
          "paragraph",
          "teeth",
          "position",
          "whole",
          "top"
        ),
        "basicbasic"
      )
    )

  def minStickers(s: Array[String], t: String): Int =
    if t.exists(char => !s.exists(_.contains(char))) then return -1
    val stickers = s.map(charCount)
    val target   = charCount(t)

    val toVisit = mutable.Queue((target, 0))
    val visited = mutable.Set(target)
    while toVisit.nonEmpty do
      val (remaining, stickersUsed) = toVisit.dequeue()
      val candidateStickers         = stickers.filter(_.keySet.intersect(remaining.keySet).nonEmpty)
      candidateStickers.foreach { sticker =>
        val mutableRemaining = mutable.Map.from(remaining)
        sticker.foreach { case (char, charCount) =>
          mutableRemaining.updateWith(char) {
            case None                                                => None
            case Some(remainingCount) if remainingCount <= charCount => None
            case Some(remainingCount)                                => Some(remainingCount - charCount)
          }
        }
        if mutableRemaining.isEmpty then return stickersUsed + 1
        val newRemaining = mutableRemaining.toMap
        if !visited.contains(newRemaining) then
          toVisit.enqueue((newRemaining, stickersUsed + 1))
          visited.add(newRemaining)
      }
    -1

  def charCount(w: String) =
    w.toCharArray.groupBy(identity).view.mapValues(_.length).toMap
