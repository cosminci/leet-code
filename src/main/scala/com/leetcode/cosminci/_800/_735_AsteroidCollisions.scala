package com.leetcode.cosminci._800

import scala.util.chaining.*

object _735_AsteroidCollisions:

  def asteroidCollision2(asteroids: Array[Int]): Array[Int] =
    if asteroids.length <= 1 then asteroids
    else
      asteroids.tail.foldLeft(Array(asteroids.head)) { (stack, asteroid) =>
        if asteroid > 0 then stack :+ asteroid
        else Iterator
          .iterate(stack)(_.dropRight(1))
          .dropWhile(_.lastOption.exists(h => h > 0 && h < asteroid.abs)).next()
          .pipe { stack =>
            if stack.lastOption.contains(asteroid.abs) then stack.dropRight(1)
            else if stack.lastOption.forall(_ < 0) then stack :+ asteroid
            else stack
          }
      }
