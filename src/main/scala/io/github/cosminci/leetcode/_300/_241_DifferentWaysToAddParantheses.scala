package io.github.cosminci.leetcode._300

object _241_DifferentWaysToAddParantheses:
  def main(args: Array[String]): Unit =
    println(diffWaysToCompute("2-1-1"))
    println(diffWaysToCompute("2*3-4*5"))
    println(diffWaysToCompute("11"))

  def diffWaysToCompute(expression: String): List[Int] =
    def eval(operand1: Int, operand2: Int, operation: Char): Int =
      operation match
        case '+' => operand1 + operand2
        case '-' => operand1 - operand2
        case _   => operand1 * operand2

    def dfs(expr: String): Seq[Int] =
      expr.indices.filterNot(expr(_).isDigit) match
        case Seq() => Seq(expr.toInt)
        case operationIndices =>
          for
            operationIdx <- operationIndices
            operand1     <- dfs(expr.substring(0, operationIdx))
            operand2     <- dfs(expr.substring(operationIdx + 1))
          yield eval(operand1, operand2, expr(operationIdx))

    dfs(expression).toList
