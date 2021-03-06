package board

class Board(val size: Int) {

  val alphabet = Array('A','B','C','D','E','F','G','H','I','J','K','L')

  val squareList = {
      val squares = new Array[Square](size * size)
      def fillArray(acc1: Int, acc2: Int, acc3: Int): Unit = {
        if (acc3 >= (size * size)) {}
        else if (acc2 > size) fillArray(acc1 + 1, 1, acc3)
        else {
          squares(acc3) = new Square(alphabet(acc1), acc2)
          fillArray(acc1, acc2 + 1, acc3 + 1)
        }
      }
      fillArray(0, 1, 0)
    squares
  }

  val ships = Array(new Ship(2),new Ship(2), new Ship(3), new Ship(3), new Ship(3), new Ship(4), new Ship(5))

  def locateShips = ships.foreach(sh => {
    println(s"Ship ${ships.indexOf(sh) + 1}:")
    sh.printSquares
  })

  def listSquares = squareList.foreach(s => println(s.toString))

  def chooseSquare(sNum: Int, index: Int, ship: Int) = {
    if (squareList(index).containsShip) false
    else {
      squareList(index).placeShip
      ships(ship).addSquare(sNum,squareList(index))
      true
    }
  }

  def shoot(square: String): String = {
    var ret = "miss"
    val target = squareList.filter(s => s.toString.equals(square)).head
    if (target.hit) {
      println("You have already shot at that square")
      ret = "fail"
    }
    else {
      target.shoot
      ships.foreach(ship =>
          ship.squaresOccupied.foreach(s => {
            if (s.toString.equals(square)) {
              ship.hit
              ret = "hit"
              if (ship.hits >= ship.length) ship.sink
            }
          })
        )
    }
    ret
  }

  def showShots = {
    squareList.foreach(sq => if (sq.hit) {
      if (sq.containsShip) println(sq.toString + ": Hit")
      else println(sq.toString + ": Miss")
    })
  }

  def isValidDist(length: Int, frontIndex: Int, backIndex: Int): Boolean = {
    val diff = Math.abs(frontIndex-backIndex)
    if (length - 1 == diff || diff == this.size * (length - 1)) true
    else false
  }

  def dispPossiblePlaces(frontIndex: Int, length: Int) = {
    val possibilities = squareList.filter(sq => isValidDist(length, frontIndex, squareList.indexOf(sq)) && sq.isInline(squareList(frontIndex)))
    possibilities.filter(sq => isBackViable(length, sq,  squareList(frontIndex)))
  }

  def getAllSquaresInLine(length: Int, square1: Square, square2: Square) = {
    val squares = new Array[Square](length)
    val (first, second) = {
      if (squareList.indexOf(square1) < squareList.indexOf(square2)) {
        (square1, square2)
      } else {
        (square2, square1)
      }
    }
    squares(0) = first
    if (first.letter.equals(second.letter)) {
      def loadArrayHorizontal(acc: Int, index: Int): Unit = {
        if (squareList(index).equals(second)) squares(acc) = squareList(index)
        else {
          squares(acc) = squareList(index)
          loadArrayHorizontal(acc + 1, index + 1)
        }
      }
      loadArrayHorizontal(1, squareList.indexOf(first) + 1)
    }
    else {
      def loadArrayVertical(acc: Int, index: Int): Unit = {
        if (squareList(index).equals(second)) squares(acc) = squareList(index)
        else {
          squares(acc) = squareList(index)
          loadArrayVertical(acc + 1, index + size)
        }
      }
      loadArrayVertical(1, squareList.indexOf(first) + size)
    }
    squares
  }

  def isBackViable(length: Int, square1: Square, square2: Square): Boolean = {
    val squares = getAllSquaresInLine(length, square1, square2)
    squares forall (sq => !sq.containsShip)
  }

}
