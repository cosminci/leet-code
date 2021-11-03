package io.github.cosminci.leetcode._800

import scala.collection.mutable

object _735_AsteroidCollisions:
  def main(args: Array[String]): Unit =
    println(asteroidCollision(Array(2, 5, -7, 2, 8, -5, 20, 10, -15)).toList)

  def asteroidCollision(asteroids: Array[Int]): Array[Int] =
    if asteroids.length <= 1 then return asteroids

    val stack = mutable.Stack.empty[Int]
    stack.push(asteroids.head)
    asteroids.tail.foreach { asteroid =>
      if asteroid > 0 then stack.push(asteroid)
      else {
        while stack.headOption.exists(h => h > 0 && h < math.abs(asteroid)) do stack.pop()
        if stack.headOption.contains(math.abs(asteroid)) then stack.pop()
        else if stack.headOption.forall(_ < 0) then stack.push(asteroid)
      }
    }

    stack.popAll().toArray
