package io.github.cosminci.leetcode._700

object _682_BaseballGame:
  def calPoints(ops: Array[String]): Int =
    ops.foldLeft(Array.empty[Int]) { (stack, op) =>
      op match
        case "+" => stack :+ stack.takeRight(2).sum
        case "D" => stack :+ stack.last * 2
        case "C" => stack.dropRight(1)
        case v   => stack :+ v.toInt
    }.sum
