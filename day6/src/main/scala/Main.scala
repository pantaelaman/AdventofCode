import scala.io.Source
import scala.collection.mutable.Queue

@main def hello: Unit = 
  val filename = "input.txt"
  val input = Source.fromFile(filename).getLines().next

  val queuep1 = Queue[Char]();
  val queuep2 = Queue[Char]();
  var isMatchedP1 = false;
  var indexp1 = -1;
  var isMatchedP2 = false;
  var indexp2 = -1;
  for (i <- 0 until input.length) {
    val curchar = input.charAt(i)
    queuep1.enqueue(curchar)
    if (queuep1.length > 4) {
      queuep1.dequeue
      if (queuep1.distinct.length == 4) {
        if (!isMatchedP1) indexp1 = i+1
        isMatchedP1 = true;
      }
    }
    queuep2.enqueue(curchar)
    if (queuep2.length > 14) {
      queuep2.dequeue
      if (queuep2.distinct.length == 14) {
        if (!isMatchedP2) indexp2 = i+1
        isMatchedP2 = true;
      }
    }
  }

  println(s"Part 1: $indexp1")
  println(s"Part 2: $indexp2")

