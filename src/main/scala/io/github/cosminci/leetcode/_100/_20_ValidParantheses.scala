package io.github.cosminci.leetcode._100

import scala.collection.mutable

object _20_ValidParantheses:

  @main def main = println(isValid("(([{[[]]}]))"))

  def isValid(s: String): Boolean =
    val prevStack = mutable.Stack.empty[Char]
    s.foreach { p =>
      if p == '(' || p == '[' || p == '{' then prevStack.push(p)
      else {
        if prevStack.isEmpty then return false
        val prev = prevStack.pop()
        if p == ')' && prev != '(' then return false
        else if p == ']' && prev != '[' then return false
        else if p == '}' && prev != '{' then return false
      }
    }
    prevStack.isEmpty
