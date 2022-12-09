fun main() {
  val filename = "input.txt"
  val lines: List<String> = java.io.File(filename).readLines()
  val trees: List<List<Int>> = lines.map { it.toList().map { it.digitToInt() } }

  val width = trees[0].size;
  val height = trees.size;

  var scenic_scores: MutableList<MutableList<Int>> = MutableList(height) { MutableList(width) { 0 } }

  var visible = (2 * width) + (2 * height) - 4
  for (x in 1..(width - 2)) {
    for (y in 1..(height - 2)) {
      val tree = trees[y][x]
      var seen = false

      var temp_y_up = y-1
      var visible_up = true
      while (temp_y_up >= 0) {
        if (trees[temp_y_up][x] >= tree) {
          visible_up = false
          break
        }
        temp_y_up -= 1
      }
      if (temp_y_up < 0) temp_y_up = 0
      if (visible_up && !seen) {
        visible += 1
        seen = true
      }

      var temp_y_down = y+1
      var visible_down = true
      while (temp_y_down <= height - 1) {
        if (trees[temp_y_down][x] >= tree) {
          visible_down = false
          break
        }
        temp_y_down += 1
      }
      if (temp_y_down > height - 1) temp_y_down = height - 1
      if (visible_down && !seen) {
        visible += 1
      }

      var temp_x_left = x-1
      var visible_left = true 
      while (temp_x_left >= 0) {
        if (trees[y][temp_x_left] >= tree) {
          visible_left = false
          break
        }
        temp_x_left -= 1
      }
      if (temp_x_left < 0) temp_x_left = 0
      if (visible_left && !seen) {
        visible += 1
      }

      var temp_x_right = x+1
      var visible_right = true
      while (temp_x_right <= width - 1) {
        if (trees[y][temp_x_right] >= tree) {
          visible_right = false
          break
        }
        temp_x_right += 1
      }
      if (temp_x_right > width - 1) temp_x_right = width - 1
      if (visible_right && !seen) {
        visible += 1
      }

      scenic_scores[y][x] = (y - temp_y_up) * (temp_y_down - y) * (x - temp_x_left) * (temp_x_right - x)
    }
  }

  val max = scenic_scores.maxOf { it.max() }

  println("Part 1: $visible")
  println("Part 2: $max")
}
