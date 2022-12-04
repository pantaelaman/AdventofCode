import scala.io.Source
import Array._

@main def hello: Unit = 
  val filename = "input.txt"
  var countp1 = 0
  var countp2 = 0
  var lineNumber = 0
  for (line <- Source.fromFile(filename).getLines) {
    val ranges = line.split(",").map(e => e.split("-").map(f => f.toInt))
    if (ranges(0)(0) <= ranges(1)(0) && ranges(0)(1) >= ranges(1)(1)) {
      countp1+=1
    } else if (ranges(1)(0) <= ranges(0)(0) && ranges(1)(1) >= ranges(0)(1)) {
      countp1+=1
    }
    val trueRanges = ranges.map(e => range(e(0), e(1)+1))
    var incCount = false
    for (n <- trueRanges(0)) {
      if (trueRanges(1).contains(n)) {
        incCount = true
      }
    }
    if (incCount) {
      countp2+=1
    }
    lineNumber+=1
  }
  print("Part 1: ")
  println(countp1)
  print("Part 2: ")
  println(countp2)

def msg = "I was compiled by Scala 3. :)"
