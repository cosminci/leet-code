package io.github.cosminci.leetcode._900

import scala.collection.mutable

object _895_MaxFrequencyStack:
  def main(args: Array[String]): Unit =
    val freqStack = new FreqStack
    freqStack.push(5)
    freqStack.push(7)
    freqStack.push(5)
    freqStack.push(7)
    freqStack.push(4)
    freqStack.push(5)
    println(freqStack.pop())
    println(freqStack.pop())
    println(freqStack.pop())
    println(freqStack.pop())

  class FreqStack():
    private var maxFrequency = 0
    private val freqBuckets  = mutable.Map.empty[Int, mutable.Stack[Int]]
    private val frequencies  = mutable.Map.empty[Int, Int]

    def push(`val`: Int) =
      val newFrequency = frequencies.getOrElse(`val`, 0) + 1
      if newFrequency > maxFrequency then maxFrequency = newFrequency
      frequencies.update(`val`, newFrequency)
      freqBuckets.getOrElseUpdate(newFrequency, mutable.Stack.empty).push(`val`)

    def pop(): Int =
      val maxFreqValues = freqBuckets(maxFrequency)
      if maxFreqValues.size == 1 then maxFrequency -= 1
      val result = maxFreqValues.pop()
      frequencies.update(result, frequencies(result) - 1)
      result
