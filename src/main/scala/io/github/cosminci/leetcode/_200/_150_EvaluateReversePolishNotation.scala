package io.github.cosminci.leetcode._200

import scala.collection.mutable

object _150_EvaluateReversePolishNotation:
  def main(args: Array[String]): Unit =
    println(evalRPN(Array("2", "1", "+", "3", "*")))
    println(evalRPN(Array("4", "13", "5", "/", "+")))
    println(evalRPN(Array("10", "6", "9", "3", "+", "-11", "*", "/", "*", "17", "+", "5", "+")))

  private def evalRPN(tokens: Array[String]): Int =
    val pending = mutable.Stack.empty[Int]

    tokens.foreach { t =>
      if t == "+" then
        val (operand2, operand1) = (pending.pop(), pending.pop())
        pending.push(operand1 + operand2)
      else if t == "-" then
        val (operand2, operand1) = (pending.pop(), pending.pop())
        pending.push(operand1 - operand2)
      else if t == "*" then
        val (operand2, operand1) = (pending.pop(), pending.pop())
        pending.push(operand1 * operand2)
      else if t == "/" then
        val (operand2, operand1) = (pending.pop(), pending.pop())
        pending.push(operand1 / operand2)
      else pending.push(t.toInt)
    }

    pending.last
