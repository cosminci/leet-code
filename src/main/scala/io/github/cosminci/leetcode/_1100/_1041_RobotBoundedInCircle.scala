package io.github.cosminci.leetcode._1100

object _1041_RobotBoundedInCircle:

  private val clockwise        = Map('N' -> 'W', 'W' -> 'S', 'S' -> 'E', 'E' -> 'N')
  private val counterClockwise = Map('N' -> 'E', 'E' -> 'S', 'S' -> 'W', 'W' -> 'N')

  def isRobotBounded(instructions: String): Boolean =
    val (d, p) = instructions.foldLeft(('N', (0, 0))) { case ((direction, pos), command) =>
      command match
        case 'L' => (clockwise(direction), pos)
        case 'R' => (counterClockwise(direction), pos)
        case 'G' => (direction, move(direction, pos))
    }
    p == (0, 0) || d != 'N'

  private def move(direction: Char, pos: (Int, Int)): (Int, Int) = direction match
    case 'N' => (pos._1, pos._2 + 1)
    case 'W' => (pos._1 + 1, pos._2)
    case 'S' => (pos._1, pos._2 - 1)
    case 'E' => (pos._1 - 1, pos._2)
