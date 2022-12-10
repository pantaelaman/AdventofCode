import kotlin.math.abs
import kotlin.math.max
import kotlin.math.min

fun clamp(v: Int, minv: Int, maxv: Int): Int {
  return min(max(v, minv), maxv)
}

class Point(x: Int, y: Int) {
  var x: Int = x
  var y: Int = y

  fun getDistancePair(point: Point): Pair<Int, Int> {
    return Pair(clamp((point.x - x), -2, 2), clamp((point.y - y), -2, 2))
  }

  override fun toString() = "($x, $y)"
}

fun main() {
  val filename = "input.txt"
  val visited_points_p1: MutableSet<Pair<Int, Int>> = mutableSetOf(Pair(0, 0));
  val visited_points_p2: MutableSet<Pair<Int, Int>> = mutableSetOf(Pair(0, 0));
  var rope = MutableList<Point>(10) { Point(0,0) }
  val head = rope[0]

  var counter = 0
  for (line in java.io.File(filename).readLines()) {
    val parts = line.split(" ")
    val direction = parts[0]
    val amount = parts[1].toInt()
    var movecounter = 0
    for (i in 0..(amount-1)) {
      if (direction == "R") {
        head.x += 1
      } else if (direction == "L") {
        head.x -= 1
      } else if (direction == "U") {
        head.y += 1
      } else if (direction == "D") {
        head.y -= 1
      }

      var previous = head
      for (j in 1..(rope.size - 1)) {
        val knot = rope[j]
        val (dx, dy) = knot.getDistancePair(previous);
        if (abs(dx) == 2) {
          knot.x += (dx/2)
          if (abs(dy) == 1) {
            knot.y += dy
          }
        } else if (abs(dy) == 2) {
          knot.y += (dy/2)
          if (abs(dx) == 1) {
            knot.x += dx
          }
        }
        previous = rope[j]
      }

      visited_points_p1.add(Pair(rope[1].x, rope[1].y))
      visited_points_p2.add(Pair(rope.last().x, rope.last().y))

      movecounter++
    }
    counter++
  }
  println(visited_points_p2)
  println("Part 1: ${visited_points_p1.size}")
  println("Part 2 (DOES NOT WORK): ${visited_points_p2.size}")
}
