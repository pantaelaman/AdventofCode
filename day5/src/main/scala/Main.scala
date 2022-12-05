import scala.io.Source
import scala.collection.mutable.Stack

@main def main: Unit =
  val filename = "input.txt"
  val lines = Source.fromFile(filename).getLines().toList
  val numberOfStacks = (lines(0).length() + 1) / 4
  val stacksp1 = Array.fill(numberOfStacks) { Stack[Char]() }
  val stacksp2 = Array.fill(numberOfStacks) { Stack[Char]() }
  
  var i = 0
  while (!lines(i).charAt(1).isDigit) { // going to assume >1 stacks
    for (j <- 0 until numberOfStacks) {
      val chr = lines(i).charAt((j * 4) + 1)
      if (chr != ' ') { // empty column
        stacksp1(j).push(chr)
        stacksp2(j).push(chr)
      }
    }
    i+=1
  }
 
  for (i <- 0 until numberOfStacks) {
    stacksp1(i) = stacksp1(i).reverse
    stacksp2(i) = stacksp2(i).reverse
  }

  i+=2 // i now points to the first line of instructions
  while (i<lines.length) {
    val instruction = lines(i).split(" ")
    val amount = instruction(1).toInt
    val fromStack = instruction(3).toInt - 1
    val toStack = instruction(5).toInt - 1
    val tmpStack = Stack[Char]()
    for (_ <- 0 until amount) {
      stacksp1(toStack).push(stacksp1(fromStack).pop)
      tmpStack.push(stacksp2(fromStack).pop)
    }
    while (!tmpStack.isEmpty) {
      stacksp2(toStack).push(tmpStack.pop)
    }
    i+=1
  }

  var msgp1 = ""
  for (stack <- stacksp1) {
    msgp1 += stack.pop
  }

  var msgp2 = ""
  for (stack <- stacksp2) {
    msgp2 += stack.pop
  }

  println(s"Part 1: $msgp1")
  println(s"Part 2: $msgp2")

