package com.leetcode.cosminci._2700

import scala.collection.mutable

object _2671_FrequencyTracker:

  class FrequencyTracker:

    private val numToFreq = mutable.Map.empty[Int, Int].withDefaultValue(0)
    private val freqToNum = mutable.Map.empty[Int, Set[Int]].withDefaultValue(Set.empty)

    def add(num: Int): Unit =
      val f = numToFreq(num)
      numToFreq.update(num, f + 1)
      if f > 0 then freqToNum.update(f, freqToNum(f) - num)
      freqToNum.update(f + 1, freqToNum(f + 1) + num)

    def deleteOne(num: Int): Unit =
      val f = numToFreq(num)
      numToFreq.updateWith(num) {
        case Some(v) if v > 1 => Some(v - 1)
        case _                => None
      }
      freqToNum.update(f, freqToNum(f) - num)
      if (freqToNum(f).isEmpty) freqToNum.remove(f)
      if f > 1 then freqToNum.update(f - 1, freqToNum(f - 1) + num)

    def hasFrequency(f: Int): Boolean =
      freqToNum(f).nonEmpty
