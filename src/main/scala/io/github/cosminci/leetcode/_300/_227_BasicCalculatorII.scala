package io.github.cosminci.leetcode._300

import scala.collection.mutable

object _227_BasicCalculatorII:
  def main(args: Array[String]): Unit =
    println(calculate("7/ 3+5 / 2 "))
    println(calculate("3+2*2"))
    println(calculate(" 3 / 2 "))

  private def calculate(s: String): Int =
    val stack                = mutable.Stack.empty[Int]
    var (operand, operation) = (0, '+')

    s.indices.foreach { i =>
      val char = s(i)
      if char.isDigit then operand = operand * 10 + (char - '0')

      if (!char.isDigit && char != ' ') || i == s.length - 1 then
        operation match
          case '+' =>
            stack.push(operand)
          case '-' =>
            stack.push(-operand)
          case '/' =>
            stack.push(stack.pop() / operand)
          case _ =>
            stack.push(stack.pop() * operand)
        operand = 0
        operation = char
    }

    stack.sum
