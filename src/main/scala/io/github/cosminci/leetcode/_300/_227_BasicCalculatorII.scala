package io.github.cosminci.leetcode._300

object _227_BasicCalculatorII:
  def main(args: Array[String]): Unit =
    println(calculate("7/ 3+5 / 2 "))
    println(calculate("3+2*2"))
    println(calculate(" 3 / 2 "))

  def calculate(s: String): Int = {
    @annotation.tailrec
    def dfs(i: Int, stack: Array[Int], operand: Int, operation: Char): Int =
      if (i == s.length) stack.sum
      else {
        val newOperand = if (s(i).isDigit) operand * 10 + s(i) - '0' else operand

        if (i != s.length - 1 && (s(i).isDigit || s(i) == ' '))
          dfs(i + 1, stack, newOperand, operation)
        else if (operation == '+')
          dfs(i + 1, stack :+ newOperand, 0, s(i))
        else if (operation == '-')
          dfs(i + 1, stack :+ -newOperand, 0, s(i))
        else if (operation == '/')
          dfs(i + 1, stack.dropRight(1) :+ stack.last / newOperand, 0, s(i))
        else
          dfs(i + 1, stack.dropRight(1) :+ stack.last * newOperand, 0, s(i))
      }

    dfs(i = 0, stack = Array.empty, operand = 0, operation = '+')
  }
