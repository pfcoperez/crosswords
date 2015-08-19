object Crosswords extends App {

  case class Cell(i: Int, j: Int)
  type Gap = List[Cell]

  def horizontalScan(sets: Vector[Vector[Char]]): List[Gap] = {
    val res = sets.zipWithIndex.flatMap { case (row: Vector[Char], i: Int) =>
      val (lasth, hs) = (row.zipWithIndex :\ (List[Cell](), List[Gap]())) {
        case ((cur: Char, j: Int), (sub: Gap, acc: List[Gap])) =>
          if(cur == '-') (Cell(i, j)::sub, acc) else  (Nil, sub :: acc)
      }
      lasth::hs
    }
    res.toList.filter(_.length > 0)
  }

  def verticalScan(sets: Vector[Vector[Char]]): List[Gap] = {
    val n = sets.size
    val m = sets.head.size
    val res = horizontalScan(for(j <- 0 until m toVector) yield for(i <- 0 until n toVector) yield sets(i)(j))
    res map { gap => gap map { cell => Cell(cell.j, cell.i)} }
  }

  def matchesGap(word: String, gap: Gap, st: Vector[Vector[Char]]): Boolean = {
    word.length == gap.length &&
      (word zip (gap.map(pos => st(pos.i)(pos.j)))).forall { case (l, hl) => l == hl || hl == '-' }
  }

  def applyWord(word: String, gap: Gap, st: Vector[Vector[Char]]): Vector[Vector[Char]] = {
    val pos2letter = (gap zip word).toMap
    for(i <- 0 until st.size toVector) yield for(j <- 0 until st(i).size toVector) yield pos2letter.getOrElse(Cell(i,j), st(i)(j))
  }

  def assignWords(words: List[String], gaps: List[Gap], st: Vector[Vector[Char]]): (Boolean, Vector[Vector[Char]]) = words match {
    case Nil =>
      (true, st)
    case _ => {
      val solutions: List[(Boolean, Vector[Vector[Char]])] = words.flatMap { w =>
        gaps.filter(gap => matchesGap(w, gap, st)) map {
          case gap =>
            assignWords(words.tail, gaps.filter(g => g != gap), applyWord(w, gap, st))
        }
      }
      solutions collectFirst { case (true, res: Vector[Vector[Char]]) => (true, res)} getOrElse (false, st)
    }
  }

  def loadBoard: Vector[Vector[Char]] = {
    for(_ <- 1 to 10 toVector) yield {
      readLine.trim toVector
    }
  }

  def loadWords: List[String] = (readLine split ";" toList).map(_.trim).sortBy(_.length).reverse

  def printBoard(board: Vector[Vector[Char]]): Unit = board.foreach(row => println(row.mkString))

  val board = loadBoard
  val words = loadWords

  val gaps = horizontalScan(board) ++ verticalScan(board)

  val (success, result) = assignWords(words, gaps, board)
  if(!success) {
    println("Couldn't find a solution!!")
    printBoard(result)
  }
  else printBoard(result)

}


/*
+-++++++++
+-++++++++
+-++++++++
+-----++++
+-+++-++++
+-+++-++++
+++++-++++
++------++
+++++-++++
+++++-++++
LONDON;DELHI;ICELAND;ANKARA
 */

/*
+-++++++++
+-++-+++++
+-------++
+-++-+++++
+-++-+++++
+-++-+++++
++++-+++++
++++-+++++
++++++++++
----------
CALIFORNIA;NIGERIA;CANADA;TELAVIV
 */