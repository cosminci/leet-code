package io.github.cosminci.leetcode._900

object _874_WalkingRobotSimulation:
  def main(args: Array[String]): Unit =
    println(robotSim(Array(4, -1, 3), Array.empty))

  private def robotSim(commands: Array[Int], obstacles: Array[Array[Int]]): Int =
    val delta = Seq((0, 1), (1, 0), (0, -1), (-1, 0))
    val obst  = Set.from(obstacles.map(arr => (arr(0), arr(1))))

    def move(x0: Int, y0: Int, dx: Int, dy: Int, moves: Int): (Int, Int) =
      var (x, y, m) = (x0, y0, moves)
      while m > 0 && !obst.contains((x + dx, y + dy)) do
        x += dx
        y += dy
        m -= 1
      (x, y)

    commands.foldLeft(0, 0, 0, 0) {
      case ((maxDistance, x, y, direction), command) =>
        val newDirection = command match
          case -1 => (direction + 1) % 4
          case -2 => (direction + 3) % 4
          case _  => direction
        val (dx, dy) = delta(newDirection)
        val (tx, ty) = move(x, y, dx, dy, command)

        (maxDistance.max(tx * tx + ty * ty), tx, ty, newDirection)
    }._1
